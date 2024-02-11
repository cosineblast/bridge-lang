use std::{
    collections::{HashMap, HashSet},
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
    type_assignments: HashMap<syntax::AstId, Type>,
    diagnostics: Vec<TypeDiagnostic>,
}

impl TypeCheck {
    // in this implementation, we have two kinds of type checking functions,
    // the check_*_raw ones and the check_* ones. their only difference, is that
    // the check_* ones call their respective check_*_raw ones, and assign the result to the
    // expression type map. One should not call the check_*_raw ones directly.
    fn check_if_type_raw(&mut self, expression: &syntax::IfExpression) -> Option<Type> {
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

    fn check_if_type(&mut self, expression: &syntax::IfExpression) -> Option<Type> {
        let result = self.check_if_type_raw(expression)?;
        self.type_assignments.insert(expression.id, result.clone());
        Some(result)
    }

    fn check_while_type_raw(&mut self, while_expression: &syntax::WhileExpression) -> Option<Type> {
        let condition_type = self.check_expression_type(&while_expression.condition)?;

        if condition_type != Type::Bool {
            self.diagnostics
                .push(TypeDiagnostic::InvalidWhileCondition(condition_type));
        }

        self.check_block_type(&while_expression.body)
    }

    fn check_while_type(&mut self, expression: &syntax::WhileExpression) -> Option<Type> {
        let result = self.check_while_type_raw(expression)?;
        self.type_assignments.insert(expression.id, result.clone());
        Some(result)
    }

    fn check_function_call_type_raw(
        &mut self,
        call: &syntax::FunctionCallExpression,
    ) -> Option<Type> {
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

    fn check_function_call_type(&mut self, call: &syntax::FunctionCallExpression) -> Option<Type> {
        let result = self.check_function_call_type_raw(call)?;
        self.type_assignments.insert(call.id, result.clone());
        Some(result)
    }

    pub fn check_expression_type_raw(&mut self, expression: &syntax::Expression) -> Option<Type> {
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

    pub fn check_expression_type(&mut self, expression: &syntax::Expression) -> Option<Type> {
        let result = self.check_expression_type_raw(expression)?;
        self.type_assignments
            .insert(expression.id(), result.clone());
        Some(result)
    }

    fn check_block_type_raw(&mut self, block: &syntax::Block) -> Option<Type> {
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

    fn check_block_type(&mut self, block: &syntax::Block) -> Option<Type> {
        let result = self.check_block_type_raw(block)?;
        self.type_assignments.insert(block.id, result.clone());
        Some(result)
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

pub struct TypeCheckOutput {
    pub type_assignments: HashMap<syntax::AstId, Type>,
}

pub fn perform_type_check(
    expression: &syntax::Expression,
) -> Result<TypeCheckOutput, Vec<TypeDiagnostic>> {
    let mut type_check = TypeCheck {
        type_assignments: HashMap::default(),
        declarations: DeclarationCounter::default(),
        diagnostics: Vec::new(),
    };

    let result = type_check.check_expression_type(expression);

    if result.is_none() || !type_check.diagnostics.is_empty() {
        Err(type_check.diagnostics)
    } else {
        Ok(TypeCheckOutput {
            type_assignments: type_check.type_assignments,
        })
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::syntax;

    #[test]
    fn test_basic_typing() {
        let expression =
            syntax::parse_expression(r#"{ let x = "hi"; let y = 2; "bruh"; x; y}"#).unwrap();

        let result = perform_type_check(&expression).unwrap();
        let types = &result.type_assignments;

        assert_eq!(types[&expression.id()], Type::Int);

        let as_block: syntax::Block = expression.try_into().unwrap();
        let first: &syntax::LetStatement = (&as_block.statements[0]).try_into().unwrap();
        assert_eq!(types[&first.value.id()], Type::String);

        let second: &syntax::LetStatement = (&as_block.statements[1]).try_into().unwrap();
        assert_eq!(types[&second.value.id()], Type::Int);

        let third: &syntax::Expression = (&as_block.statements[2]).try_into().unwrap();
        assert_eq!(types[&third.id()], Type::String);

        let fourth: &syntax::Expression = (&as_block.statements[3]).try_into().unwrap();
        assert_eq!(types[&fourth.id()], Type::String);

        let last: &syntax::Expression = (&as_block.statements[4]).try_into().unwrap();
        assert_eq!(types[&last.id()], Type::Int);
    }
}
