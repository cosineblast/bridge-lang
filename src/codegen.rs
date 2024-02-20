use crate::semantic;
use std::{fmt::Display, ptr::null_mut};

use crate::{semantic::TypeCheckOutput, syntax};
use llvm::{core::{LLVMAddIncoming, LLVMBuildPhi}, LLVMValue};
use llvm_sys as llvm;

use crate::common::DeclarationCounter;

// We are going to avoid leaking llvm-sys stuff for now
pub struct Codegen {
    llvm_context: *mut llvm::LLVMContext,
    llvm_module: *mut llvm::LLVMModule,
    llvm_builder: *mut llvm::LLVMBuilder,
    types: Option<TypeCheckOutput>,
    generated_count: u32,
    declarations: DeclarationCounter<*mut LLVMValue>,
}

impl Codegen {
    pub fn new() -> anyhow::Result<Self> {
        unsafe {
            let llvm_context = llvm::core::LLVMContextCreate();
            let llvm_module = llvm::core::LLVMModuleCreateWithNameInContext(
                b"repl\0".as_ptr() as *const i8,
                llvm_context,
            );
            let llvm_builder = llvm::core::LLVMCreateBuilderInContext(llvm_context);

            Ok(Self {
                llvm_context,
                llvm_module,
                llvm_builder,
                types: None,
                generated_count: 0,
                declarations: DeclarationCounter::default(),
            })
        }
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
                self.llvm_module,
                function_name.as_ptr() as *const i8,
                function_type,
            )
        };

        let block: *mut llvm::LLVMBasicBlock = unsafe {
            llvm::core::LLVMAppendBasicBlockInContext(
                self.llvm_context,
                function,
                b"entry\0".as_ptr() as *const i8,
            )
        };

        unsafe {
            llvm::core::LLVMPositionBuilderAtEnd(self.llvm_builder, block);
        }

        let result_value = self.gen_expression(expression)?;

        unsafe {
            llvm::core::LLVMBuildRet(self.llvm_builder, result_value);
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
            semantic::Type::Int => unsafe { llvm::core::LLVMInt64TypeInContext(self.llvm_context) },
            semantic::Type::Bool => unsafe { llvm::core::LLVMInt1TypeInContext(self.llvm_context) },
            semantic::Type::String => todo!(),
            semantic::Type::Unit => unsafe { llvm::core::LLVMVoidTypeInContext(self.llvm_context) },
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
                let int_type = llvm::core::LLVMInt64TypeInContext(self.llvm_context);

                let result = llvm::core::LLVMConstInt(int_type, i as u64, true as i32);

                Ok(result)
            },

            syntax::Literal::Bool(b) => unsafe {
                let int_type = llvm::core::LLVMInt1TypeInContext(self.llvm_context);

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
                llvm::core::LLVMGetInsertBlock(self.llvm_builder),
            );

            let then_block = llvm::core::LLVMAppendBasicBlockInContext(
                self.llvm_context,
                current_function,
                b"then\0".as_ptr() as *const i8,
            );

            let else_block = llvm::core::LLVMAppendBasicBlockInContext(
                self.llvm_context,
                current_function,
                b"else\0".as_ptr() as *const i8,
            );

            let end_block = llvm::core::LLVMAppendBasicBlockInContext(
                self.llvm_context,
                current_function,
                b"end\0".as_ptr() as *const i8,
            );

            llvm::core::LLVMBuildCondBr(self.llvm_builder, condition_value, then_block, else_block);

            // build then branch
            llvm::core::LLVMPositionBuilderAtEnd(self.llvm_builder, then_block);
            let then_value = self.gen_block(&expression.then_branch)?;
            llvm::core::LLVMBuildBr(self.llvm_builder, end_block);

            // we reset the then_block because phi thingies
            // TODO: study this
            let then_block = llvm::core::LLVMGetInsertBlock(self.llvm_builder);

            // build else branch

            llvm::core::LLVMPositionBuilderAtEnd(self.llvm_builder, else_block);


            let else_value = self.gen_block(expression.else_branch.as_ref().expect("else not implemented"))?;
            llvm::core::LLVMBuildBr(self.llvm_builder, end_block);

            let else_block = llvm::core::LLVMGetInsertBlock(self.llvm_builder);

            // end block

            llvm::core::LLVMPositionBuilderAtEnd(self.llvm_builder, end_block);

            let phi = LLVMBuildPhi(self.llvm_builder,
                                   self.llvm_type_of(self.type_of(expression.id)),
                                   b"bruh\0".as_ptr() as *const i8
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

impl Drop for Codegen {
    fn drop(&mut self) {
        unsafe {
            llvm::core::LLVMDisposeBuilder(self.llvm_builder);
            llvm::core::LLVMDisposeModule(self.llvm_module);
            llvm::core::LLVMContextDispose(self.llvm_context);
        }
    }
}

type IRResult = anyhow::Result<*mut llvm::LLVMValue>;
