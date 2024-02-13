
use std::fmt::Display;

use llvm_sys as llvm;
use crate::{semantic::TypeCheckOutput, syntax};

// We are going to avoid leaking llvm-sys stuff for now
pub struct Codegen {
    llvm_context: *mut llvm::LLVMContext,
    llvm_module: *mut llvm::LLVMModule,
    llvm_builder: *mut llvm::LLVMBuilder,
    types: Option<TypeCheckOutput>,
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
            })
        }
    }

    pub fn codegen_expression(&mut self, expression: &syntax::Expression, type_check: TypeCheckOutput) -> anyhow::Result<ExpressionOutput> {
        self.types = Some(type_check);

        let ir = match expression {
            syntax::Expression::Literal(literal) => literal.codegen(self)?,
            _ => todo!()
        };

        Ok(ExpressionOutput {
            generated_ir: ir,
        })
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

unsafe trait HasCodegen {
    fn codegen(&self, context: &mut Codegen) -> IRResult;
}

unsafe impl HasCodegen for syntax::LiteralExpression {
    fn codegen(&self, context: &mut Codegen) -> IRResult {
        match self.literal {
            syntax::Literal::Integer(i) => {
                unsafe {
                    let int_type = llvm::core::LLVMInt64TypeInContext(context.llvm_context);

                    let result = llvm::core::LLVMConstInt(int_type, i as u64, true as i32);

                    Ok(result)
                }
            },

            syntax::Literal::Bool(b) => {
                unsafe {
                    let int_type = llvm::core::LLVMInt1TypeInContext(context.llvm_context);

                    let result = llvm::core::LLVMConstInt(int_type, b as u64, true as i32);

                    Ok(result)
                }
            }
            _ => todo!()
        }
    }
}
