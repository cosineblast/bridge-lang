use std::{
    collections::HashSet,
    fmt::{Debug, Display},
};

use thiserror::Error;

use crate::syntax;

use super::common::DeclarationCounter;

#[derive(Debug, PartialEq, Clone)]
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

    #[error("The type specifier defines type `{0}` but has type `{1}` instead")]
    MismatchedLetType(Type, Type),
}

pub struct TypeCheck {
    declarations: DeclarationCounter<Type>,
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
        use syntax as s;
        let result = match expression {
            s::Expression::If(expression) => self.check_if_type(expression)?,
            s::Expression::Literal(literal) => match literal {
                s::LiteralExpression {
                    literal: s::Literal::Integer(_),
                    ..
                } => Type::Int,
                s::LiteralExpression {
                    literal: s::Literal::String(_),
                    ..
                } => Type::String,
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

    fn check_block_type(&mut self, block: &syntax::Block) -> Option<Type> {
        let mut last: Option<Type> = None;
        let mut local_declarations = HashSet::new();

        for statement in block.statements.iter() {
            match statement {
                syntax::Statement::Expression(expression) => {
                    last = Some(self.check_expression_type(expression)?);
                }

                syntax::Statement::Let(declaration) => {
                    let let_type = declaration
                        .type_specifier
                        .as_ref()
                        .and_then(|it| self.check_type_from_name(it));
                    let expression_type = self.check_expression_type(&declaration.value)?;

                    if let Some(let_type) = let_type {
                        if let_type != expression_type {
                            self.diagnostics
                                .push(TypeDiagnostic::MismatchedLetType(expression_type, let_type));

                            return None;
                        }
                    }

                    let name = &declaration.name.symbol;
                    self.declarations.add_with(name, expression_type);
                    local_declarations.insert(name);

                    last = None;
                }
            }
        }

        for declaration in local_declarations {
            self.declarations.subtract(declaration);
        }

        if let Some(last) = last {
            Some(last)
        } else {
            Some(Type::Unit)
        }
    }

    fn check_type_from_name(&mut self, source: &syntax::Type) -> Option<Type> {
        match source.name.as_str() {
            "Int" => Some(Type::Int),
            "Bool" => Some(Type::Bool),
            "String" => Some(Type::String),
            "Unit" => Some(Type::Unit),
            _ => None,
        }
    }

    fn check_identifier_type(&mut self, identifier: &syntax::Identifier) -> Option<Type> {
        if let Some(result) = self.declarations.get(&identifier.symbol) {
            Some(result.clone())
        } else {
            panic!("Unknown identifier in type analysis: {}", identifier.symbol);
        }
    }
}
