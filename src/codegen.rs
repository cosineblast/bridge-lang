use crate::semantic;
use std::{fmt::Display, pin::Pin};

use crate::{semantic::TypeCheckOutput, syntax};
use llvm::{
    core::{LLVMAddIncoming, LLVMBuildPhi},
    LLVMValue,
};
use llvm_sys as llvm;

use crate::common::DeclarationCounter;

use inkwell::context::AsContextRef;

pub struct Codegen {
    llvm_module: inkwell::module::Module<'static>,
    llvm_builder: inkwell::builder::Builder<'static>,

    // it is VERY important that this is after llvm_module and llvm_builder
    llvm_context: Pin<Box<inkwell::context::Context>>,

    types: Option<TypeCheckOutput>,
    generated_count: u32,
    declarations: DeclarationCounter<*mut LLVMValue>,
}

impl Codegen {
    pub fn new() -> anyhow::Result<Self> {
        let llvm_context = inkwell::context::Context::create();
        let llvm_context = Box::pin(llvm_context);

        // SAFETY:
        // well, the LLVM context works like an allocator arena,
        // and module and builder are objects originated from such arena,
        // so you need to fill their lifetimes with something.
        // Well, we fill them with static as a placeholder,
        // but in fact, it points to the pinned llvm context object.
        // I believe this to be safe, because order of object destruction
        // is guaranteed to be in order of declaration
        // in rust, so the _possibly_¹ existing references in
        // the module and builder don't actually exist by the time the context is
        // dropped.
        // This means that the unbounded lifetime created by this unsafe block
        // never refers to dangling stuff
        let llvm_context_ref =
            unsafe { &*(llvm_context.as_ref().get_ref() as *const inkwell::context::Context) };

        Ok(Self {
            llvm_context,
            llvm_module: llvm_context_ref.create_module("repl"),
            llvm_builder: llvm_context_ref.create_builder(),
            types: None,
            generated_count: 0,
            declarations: DeclarationCounter::default(),
        })
    }

    fn llvm_context_raw(&mut self) -> *mut llvm::LLVMContext {
        (&*self.llvm_context).as_ctx_ref()
    }

    fn llvm_module_raw(&mut self) -> *mut llvm::LLVMModule {
        (&self.llvm_module).as_mut_ptr()
    }

    fn llvm_builder_raw(&mut self) -> *mut llvm::LLVMBuilder {
        (&self.llvm_builder).as_mut_ptr()
    }

    pub fn codegen_expression(
        &mut self,
        expression: &syntax::Expression,
        type_check: TypeCheckOutput,
    ) -> anyhow::Result<ExpressionOutput> {
        self.types = Some(type_check);

        let function_type = {
            let return_type = self.llvm_type_of(self.type_of(expression.id()));
            unsafe { llvm::core::LLVMFunctionType(return_type, std::ptr::null_mut(), 0, 0) }
        };

        let function = unsafe {
            let function_name = format!("repl_{}\0", self.generated_count);
            self.generated_count += 1;
            llvm::core::LLVMAddFunction(
                self.llvm_module_raw(),
                function_name.as_ptr() as *const i8,
                function_type,
            )
        };

        let block: *mut llvm::LLVMBasicBlock = unsafe {
            llvm::core::LLVMAppendBasicBlockInContext(
                self.llvm_context_raw(),
                function,
                b"entry\0".as_ptr() as *const i8,
            )
        };

        unsafe {
            llvm::core::LLVMPositionBuilderAtEnd(self.llvm_builder_raw(), block);
        }

        let result_value = self.gen_expression(expression)?;

        unsafe {
            llvm::core::LLVMBuildRet(self.llvm_builder_raw(), result_value);
            llvm::analysis::LLVMVerifyFunction(
                function,
                llvm::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction,
            );
        }

        Ok(ExpressionOutput {
            generated_ir: function,
        })
    }

    fn type_of(&self, id: syntax::AstId) -> semantic::Type {
        self.types.as_ref().unwrap().type_assignments[&id].clone()
    }

    fn llvm_type_of(&mut self, ty: semantic::Type) -> *mut llvm::LLVMType {
        match ty {
            semantic::Type::Int => unsafe {
                llvm::core::LLVMInt64TypeInContext(self.llvm_context_raw())
            },
            semantic::Type::Bool => unsafe {
                llvm::core::LLVMInt1TypeInContext(self.llvm_context_raw())
            },
            semantic::Type::String => todo!(),
            semantic::Type::Unit => unsafe {
                llvm::core::LLVMVoidTypeInContext(self.llvm_context_raw())
            },
            semantic::Type::Function(_, _) => todo!(),
        }
    }

    fn gen_expression(&mut self, expression: &syntax::Expression) -> IRResult {
        // TODO: remove effect from functions, as codegen is currently greedy
        if let semantic::Type::Unit = self.type_of(expression.id()) {
            return Ok(std::ptr::null_mut());
        }

        match expression {
            syntax::Expression::Literal(literal) => self.gen_literal(literal),
            syntax::Expression::Block(block) => self.gen_block(&block.0),
            syntax::Expression::Identifier(identifier) => self.gen_identifier(identifier),
            syntax::Expression::If(expr) => self.gen_if(expr),
            _ => todo!(),
        }
    }

    fn gen_literal(&mut self, literal: &syntax::LiteralExpression) -> IRResult {
        match literal.literal {
            syntax::Literal::Integer(i) => unsafe {
                let int_type = llvm::core::LLVMInt64TypeInContext(self.llvm_context_raw());

                let result = llvm::core::LLVMConstInt(int_type, i as u64, true as i32);

                Ok(result)
            },

            syntax::Literal::Bool(b) => unsafe {
                let int_type = llvm::core::LLVMInt1TypeInContext(self.llvm_context_raw());

                let result = llvm::core::LLVMConstInt(int_type, b as u64, true as i32);

                Ok(result)
            },
            _ => todo!(),
        }
    }

    fn gen_block(&mut self, block: &syntax::Block) -> IRResult {
        let mut last_value = None;

        self.declarations.start_block();

        for statement in block.statements.iter() {
            match statement {
                syntax::Statement::Let(statement) => {
                    let result = self.gen_expression(&statement.expression)?;

                    self.declarations.add_with(&statement.name.symbol, result);
                }

                syntax::Statement::Expression(expression) => {
                    last_value = Some(self.gen_expression(expression)?);
                }
            }
        }

        self.declarations.end_block();

        Ok(last_value.unwrap_or(std::ptr::null_mut()))
    }

    fn gen_identifier(&mut self, block: &syntax::IdentifierExpression) -> IRResult {
        let value = self.declarations.get(&block.0.symbol).unwrap();

        Ok(*value)
    }

    fn gen_if(&mut self, expression: &syntax::IfExpression) -> IRResult {
        unsafe {
            let condition_value = self.gen_expression(&expression.condition)?;

            let current_function = llvm::core::LLVMGetBasicBlockParent(
                llvm::core::LLVMGetInsertBlock(self.llvm_builder_raw()),
            );

            let then_block = llvm::core::LLVMAppendBasicBlockInContext(
                self.llvm_context_raw(),
                current_function,
                b"then\0".as_ptr() as *const i8,
            );

            let else_block = llvm::core::LLVMAppendBasicBlockInContext(
                self.llvm_context_raw(),
                current_function,
                b"else\0".as_ptr() as *const i8,
            );

            let end_block = llvm::core::LLVMAppendBasicBlockInContext(
                self.llvm_context_raw(),
                current_function,
                b"end\0".as_ptr() as *const i8,
            );

            llvm::core::LLVMBuildCondBr(
                self.llvm_builder_raw(),
                condition_value,
                then_block,
                else_block,
            );

            // build then branch
            llvm::core::LLVMPositionBuilderAtEnd(self.llvm_builder_raw(), then_block);
            let then_value = self.gen_block(&expression.then_branch)?;
            llvm::core::LLVMBuildBr(self.llvm_builder_raw(), end_block);

            // we reset the then_block because phi thingies
            // TODO: study this
            let then_block = llvm::core::LLVMGetInsertBlock(self.llvm_builder_raw());

            // build else branch

            llvm::core::LLVMPositionBuilderAtEnd(self.llvm_builder_raw(), else_block);

            let else_value = self.gen_block(
                expression
                    .else_branch
                    .as_ref()
                    .expect("else not implemented"),
            )?;
            llvm::core::LLVMBuildBr(self.llvm_builder_raw(), end_block);

            let else_block = llvm::core::LLVMGetInsertBlock(self.llvm_builder_raw());

            // end block

            llvm::core::LLVMPositionBuilderAtEnd(self.llvm_builder_raw(), end_block);

            let phi = LLVMBuildPhi(
                self.llvm_builder_raw(),
                self.llvm_type_of(self.type_of(expression.id)),
                b"bruh\0".as_ptr() as *const i8,
            );

            let mut values = [then_value, else_value];
            let mut blocks = [then_block, else_block];

            LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);

            Ok(phi)
        }
    }
}

pub struct ExpressionOutput {
    generated_ir: *mut llvm::LLVMValue,
}

impl Display for ExpressionOutput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            let ir_string = llvm::core::LLVMPrintValueToString(self.generated_ir);
            let c_str = std::ffi::CStr::from_ptr(ir_string);
            let string = c_str.to_str().unwrap();

            write!(f, "{}", string)?;

            llvm::core::LLVMDisposeMessage(ir_string);

            Ok(())
        }
    }
}

type IRResult = anyhow::Result<*mut llvm::LLVMValue>;
