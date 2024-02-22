use crate::semantic;
use std::{fmt::Display, pin::Pin};

use crate::{semantic::TypeCheckOutput, syntax};
use llvm::{
    core::{LLVMAddIncoming, LLVMBuildPhi},
    LLVMValue,
};
use llvm_sys as llvm;

use crate::common::DeclarationCounter;

use inkwell::{
    context::AsContextRef,
    types::{AsTypeRef, BasicType, BasicTypeEnum},
    values::{AnyValue, AsValueRef, GlobalValue},
};

pub struct Codegen {
    state: CodegenState<'static>,
    _context: Pin<Box<inkwell::context::Context>>,
}

impl Codegen {
    pub fn new() -> anyhow::Result<Self> {
        let context = inkwell::context::Context::create();
        let context = Box::pin(context);
        let context_ref = unsafe {
            &*(context.as_ref().get_ref() as *const inkwell::context::Context as *const ()
                as *const inkwell::context::Context)
        };

        let state = CodegenState::new(context_ref).unwrap();

        Ok(Codegen {
            state,
            _context: context,
        })
    }

    pub fn codegen_expression(
        &mut self,
        expression: &syntax::Expression,
        type_check: TypeCheckOutput,
    ) -> anyhow::Result<ExpressionOutput> {
        self.state.codegen_expression(expression, type_check)
    }
}

pub struct CodegenState<'ctx> {
    context: &'ctx inkwell::context::Context,
    module: inkwell::module::Module<'ctx>,
    builder: inkwell::builder::Builder<'ctx>,

    types: Option<TypeCheckOutput>,
    generated_count: u32,
    declarations: DeclarationCounter<*mut LLVMValue>,
}

impl<'ctx> CodegenState<'ctx> {
    pub fn new(context: &'ctx inkwell::context::Context) -> anyhow::Result<Self> {
        Ok(Self {
            context,
            module: context.create_module("repl"),
            builder: context.create_builder(),
            types: None,
            generated_count: 0,
            declarations: DeclarationCounter::default(),
        })
    }

    fn llvm_context_raw(&self) -> *mut llvm::LLVMContext {
        self.context.as_ctx_ref()
    }

    fn llvm_builder_raw(&self) -> *mut llvm::LLVMBuilder {
        self.builder.as_mut_ptr()
    }

    pub fn codegen_expression<'a>(
        &'a mut self,
        expression: &syntax::Expression,
        type_check: TypeCheckOutput,
    ) -> anyhow::Result<ExpressionOutput<'ctx>> {
        self.types = Some(type_check);

        self.generated_count += 1;

        let function_ = {
            let function_type = {
                let return_type = self
                    .basic_llvm_type_of(self.type_of(expression.id()))
                    .unwrap();
                return_type.fn_type(&[], false)
            };

            let function_name = format!("repl_{}", self.generated_count);

            self.module
                .add_function(&function_name, function_type, None)
        };

        let block = self.context.append_basic_block(function_, "fn_entry");

        self.builder.position_at_end(block);

        let result_value = self.gen_expression(expression)?;

        unsafe {
            llvm::core::LLVMBuildRet(self.llvm_builder_raw(), result_value);
        }
        assert!(function_.verify(false));

        Ok(ExpressionOutput {
            generated_ir: function_.as_global_value(),
        })
    }

    fn type_of(&self, id: syntax::AstId) -> semantic::Type {
        self.types.as_ref().unwrap().type_assignments[&id].clone()
    }

    fn basic_llvm_type_of(&mut self, ty: semantic::Type) -> Option<BasicTypeEnum<'ctx>> {
        let result = match ty {
            semantic::Type::Int => self.context.i64_type().as_basic_type_enum(),
            semantic::Type::Bool => self.context.bool_type().as_basic_type_enum(),
            semantic::Type::String => todo!(),
            semantic::Type::Unit => return None,
            semantic::Type::Function(_, _) => return None,
        };

        Some(result)
    }

    fn llvm_type_of(&mut self, ty: semantic::Type) -> *mut llvm::LLVMType {
        self.basic_llvm_type_of(ty).unwrap().as_type_ref()
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
            syntax::Literal::Integer(i) => {
                let result = self
                    .context
                    .i64_type()
                    .const_int(i as u64, true)
                    .as_value_ref();

                Ok(result)
            }

            syntax::Literal::Bool(b) => {
                let result = self
                    .context
                    .bool_type()
                    .const_int(b as u64, true)
                    .as_value_ref();

                Ok(result)
            }
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

pub struct ExpressionOutput<'a> {
    generated_ir: GlobalValue<'a>,
}

impl<'a> Display for ExpressionOutput<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = self.generated_ir.print_to_string().to_string();

        write!(f, "{}", string.to_string())?;

        Ok(())
    }
}

type IRResult = anyhow::Result<*mut llvm::LLVMValue>;
