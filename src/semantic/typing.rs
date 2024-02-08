use std::fmt::{Debug, Display};

use thiserror::Error;

use crate::syntax;

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Bool,
    String,
    Unit,
    Function(Vec<Type>, Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

type Map<K, V> = std::collections::HashMap<K, V>;

#[derive(Debug, Error)]
pub enum TypeDiagnostic {
    #[error("Unknown type `{0}`")]
    UnknownType(String),

    #[error("Expected bool in while condition, got `{0}`")]
    InvalidWhileCondition(Type),

    #[error("Expected bool in if condition, got `{0}`")]
    InvalidIfCondition(Type),

    #[error("The then branch of the if has type `{0}` but the else has type `{1}`")]
    InconsistentIfBranches(Type, Type),

    #[error("Expected `{0}` number of arguments, got `{1}`")]
    InvalidCallArgumentCount(usize, usize),

    #[error("Expected function type, but got `{0}`")]
    NonFunctionCall(Type),

    // TODO: implement argument number and store function call, if possible
    #[error("The argument `{0}` should be of type `{0}` but is `{1}` instead")]
    MismatchedArgumentType(Type, Type),
}

pub struct TypeCheck {
    _current_type_bindings: Map<String, Type>,
    diagnostics: Vec<TypeDiagnostic>,
}

impl TypeCheck {
    fn check_if_type(&mut self, expression: &syntax::IfExpression) -> Option<Type> {
        let condition_type = self.check_expression_type(&expression.condition)?;

        if condition_type != Type::Bool {
            self.diagnostics
                .push(TypeDiagnostic::InvalidIfCondition(condition_type));
        }

        let then_type = self.check_block_type(&expression.then_branch)?;

        if let Some(else_branch) = &expression.else_branch {
            let else_type = self.check_block_type(else_branch)?;

            if then_type != else_type {
                self.diagnostics
                    .push(TypeDiagnostic::InconsistentIfBranches(then_type, else_type));
                return None;
            }
        }

        Some(Type::Unit)
    }

    fn check_while_type(&mut self, while_expression: &syntax::WhileExpression) -> Option<Type> {
        let condition_type = self.check_expression_type(&while_expression.condition)?;

        if condition_type != Type::Bool {
            self.diagnostics
                .push(TypeDiagnostic::InvalidWhileCondition(condition_type));
        }

        self.check_block_type(&while_expression.body)
    }

    fn check_function_call_type(&mut self, call: &syntax::FunctionCallExpression) -> Option<Type> {
        let function_type = self.check_identifier_type(&call.name)?;

        match function_type {
            Type::Function(parameters, result) => {
                // TODO: implement currying
                if parameters.len() != call.arguments.len() {
                    self.diagnostics
                        .push(TypeDiagnostic::InvalidCallArgumentCount(
                            parameters.len(),
                            call.arguments.len(),
                        ));
                    return None;
                }

                for (argument, parameter) in call.arguments.iter().zip(parameters) {
                    let argument_type = self.check_expression_type(argument)?;
                    if argument_type != parameter {
                        self.diagnostics
                            .push(TypeDiagnostic::MismatchedArgumentType(
                                argument_type,
                                parameter,
                            ))
                    }
                }

                Some(*result)
            }
            _ => {
                self.diagnostics
                    .push(TypeDiagnostic::NonFunctionCall(function_type));
                None
            }
        }
    }

    pub fn check_expression_type(&mut self, expression: &syntax::Expression) -> Option<Type> {
        let result = match expression {
            syntax::Expression::If(expression) => self.check_if_type(expression)?,
            syntax::Expression::Literal(literal) => match literal {
                syntax::LiteralExpression::Integer(_) => Type::Int,
                syntax::LiteralExpression::String(_) => Type::String,
            },
            syntax::Expression::While(while_expression) => {
                self.check_while_type(while_expression)?
            }
            syntax::Expression::Block(block) => self.check_block_type(&block.0)?,
            syntax::Expression::FunctionCall(call) => self.check_function_call_type(call)?,
            syntax::Expression::Identifier(identifier) => {
                self.check_identifier_type(&identifier.0)?
            }
        };

        Some(result)
    }

    fn check_block_type(&mut self, _block: &syntax::Block) -> Option<Type> {
        todo!()
    }

    fn check_identifier_type(&mut self, _identifier: &syntax::Identifier) -> Option<Type> {
        todo!()
    }
}
