
use crate::semantic;
use std::fmt::Display;

use llvm_sys as llvm;
use crate::{semantic::TypeCheckOutput, syntax};

// We are going to avoid leaking llvm-sys stuff for now
pub struct Codegen {
    llvm_context: *mut llvm::LLVMContext,
    llvm_module: *mut llvm::LLVMModule,
    llvm_builder: *mut llvm::LLVMBuilder,
    types: Option<TypeCheckOutput>,
    generated_count: u32
}

impl Codegen {
    pub fn new() -> anyhow::Result<Self> {
        unsafe {
            let llvm_context = llvm::core::LLVMContextCreate();
            let llvm_module = llvm::core::LLVMModuleCreateWithNameInContext(b"repl\0".as_ptr() as *const i8, llvm_context);
            let llvm_builder = llvm::core::LLVMCreateBuilderInContext(llvm_context);

            Ok(Self {
                llvm_context,
                llvm_module,
                llvm_builder,
                types: None,
                generated_count: 0,
            })
        }
    }

    pub fn codegen_expression(&mut self, expression: &syntax::Expression, type_check: TypeCheckOutput) -> anyhow::Result<ExpressionOutput> {
        self.types = Some(type_check);

        let function_type = {
            let return_type = self.llvm_type_of(self.type_of(expression));
            unsafe { llvm::core::LLVMFunctionType(return_type, std::ptr::null_mut(), 0, 0) }
        };

        let function = unsafe {
            let function_name = format!("repl_{}", self.generated_count);
            self.generated_count += 1;
            llvm::core::LLVMAddFunction(self.llvm_module, function_name.as_ptr() as *const i8, function_type)
        };

        let block: *mut llvm::LLVMBasicBlock = unsafe {
            llvm::core::LLVMAppendBasicBlockInContext(self.llvm_context, function, b"entry\0".as_ptr() as *const i8)
        };

        unsafe {
            llvm::core::LLVMPositionBuilderAtEnd(self.llvm_builder, block);
        }

        let result_value = self.gen_expression(expression)?;

        unsafe {
            llvm::core::LLVMBuildRet(self.llvm_builder, result_value);
            llvm::analysis::LLVMVerifyFunction(function, llvm::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction);
        }

        Ok(ExpressionOutput {
            generated_ir: function,
        })
    }

    fn type_of(&self, expression: &syntax::Expression) -> semantic::Type {
        self.types.as_ref().unwrap().type_assignments[&expression.id()].clone()
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
        if let semantic::Type::Unit = self.type_of(expression) {
            return Ok(std::ptr::null_mut());
        }

        match expression {
            syntax::Expression::Literal(literal) => self.gen_literal(literal),
            _ => todo!()
        }
    }


    fn gen_literal(&mut self, literal: &syntax::LiteralExpression) -> IRResult {
        match literal.literal {
            syntax::Literal::Integer(i) => {
                unsafe {
                    let int_type = llvm::core::LLVMInt64TypeInContext(self.llvm_context);

                    let result = llvm::core::LLVMConstInt(int_type, i as u64, true as i32);

                    Ok(result)
                }
            },

            syntax::Literal::Bool(b) => {
                unsafe {
                    let int_type = llvm::core::LLVMInt1TypeInContext(self.llvm_context);

                    let result = llvm::core::LLVMConstInt(int_type, b as u64, true as i32);

                    Ok(result)
                }
            }
            _ => todo!()
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

