use super::common::DeclarationCounter;
use crate::syntax;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum VariableUsageDiagnostic {
    #[error("This identifier is unknown")]
    UnknownIdentifier(String),
}

type Set<T> = std::collections::HashSet<T>;

struct SymbolDefinitionCheckState {
    global_symbols: Set<String>,
    declarations: DeclarationCounter,
    diagnostics: Vec<VariableUsageDiagnostic>,
}

impl SymbolDefinitionCheckState {
    fn init() -> SymbolDefinitionCheckState {
        SymbolDefinitionCheckState {
            global_symbols: super::PRELUDE.keys().cloned().collect(),
            declarations: DeclarationCounter::default(),
            diagnostics: vec![],
        }
    }

    fn check_expression(&mut self, expression: &syntax::Expression) {
        match expression {
            syntax::Expression::Literal(_) => {
                // no problem here officer
                // ~~unless we add string interpolation~~
            }
            syntax::Expression::If(if_expression) => {
                self.check_expression(&if_expression.condition);
                self.check_block(&if_expression.then_branch);

                if let Some(else_branch) = &if_expression.else_branch {
                    self.check_block(else_branch);
                }
            }
            syntax::Expression::While(while_expression) => {
                self.check_expression(&while_expression.condition);
                self.check_block(&while_expression.body);
            }
            syntax::Expression::Block(block) => {
                self.check_block(&block.0);
            }
            syntax::Expression::FunctionCall(call) => {
                // TODO: handle function call expression whose functions
                // aren't identifiers

                self.check_identifier(call.name.symbol.to_owned());

                for argument in call.arguments.iter() {
                    self.check_expression(argument);
                }
            }
            syntax::Expression::Identifier(identifier) => {
                self.check_identifier(identifier.0.symbol.clone());
            }
        }
    }

    fn check_identifier(&mut self, name: String) {
        if !self.declarations.contains(&name) && !self.global_symbols.contains(&name) {
            self.diagnostics
                .push(VariableUsageDiagnostic::UnknownIdentifier(name));
        }
    }

    fn check_block(&mut self, block: &syntax::Block) {
        let mut block_declarations = Set::default();

        for statement in block.statements.iter() {
            match statement {
                syntax::Statement::Expression(expression) => {
                    self.check_expression(expression);
                }
                syntax::Statement::Let(declaration) => {
                    self.check_expression(&declaration.expression);

                    if block_declarations.insert(declaration.name.symbol.clone()) {
                        self.declarations.add(&declaration.name.symbol);
                    }
                }
            }
        }

        for declaration in block_declarations.iter() {
            self.declarations.subtract(declaration);
        }
    }

    fn check_function_declaration(&mut self, function_declaration: &syntax::FunctionDeclaration) {
        // TODO: add module identifiers to global_symbols

        for parameter in function_declaration.parameters.iter() {
            self.declarations.add(&parameter.0.symbol);
        }

        self.check_block(&function_declaration.body);
    }

    fn add_module_identifiers(&mut self, module: &syntax::Module) {
        for top_level in module.declarations.iter() {
            match top_level {
                syntax::TopLevelDeclaration::Function(function_declaration) => {
                    self.global_symbols
                        .insert(function_declaration.name.symbol.clone());
                }
            }
        }
    }
}

pub fn analyze_module_variable_usage(
    module: &syntax::Module,
) -> Result<(), Vec<VariableUsageDiagnostic>> {
    let mut state = SymbolDefinitionCheckState::init();

    state.add_module_identifiers(module);

    for top_level in module.declarations.iter() {
        match top_level {
            syntax::TopLevelDeclaration::Function(function_declaration) => {
                state.check_function_declaration(function_declaration)
            }
        }
    }

    if state.diagnostics.is_empty() {
        Ok(())
    } else {
        Err(state.diagnostics)
    }
}

pub fn analyze_expression_variable_usage(
    expression: &syntax::Expression,
) -> Result<(), Vec<VariableUsageDiagnostic>> {
    let mut state = SymbolDefinitionCheckState::init();

    state.check_expression(expression);

    if state.diagnostics.is_empty() {
        Ok(())
    } else {
        Err(state.diagnostics)
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::syntax;

    #[test]
    fn test_simple_usage() {
        let src = r#"
        effect fn foo() {
            let x = 1;
            if (x) { x } { y; z };
            jooj()
        }
        "#;

        let module = syntax::parse_module(src).unwrap();

        let result = analyze_module_variable_usage(&module);

        assert_eq!(
            result,
            Err(vec![
                VariableUsageDiagnostic::UnknownIdentifier("y".to_string()),
                VariableUsageDiagnostic::UnknownIdentifier("z".to_string()),
                VariableUsageDiagnostic::UnknownIdentifier("jooj".to_string())
            ])
        );
    }

    #[test]
    fn test_shadowing() {
        let src = r#"
        effect fn bar() {
            let x = 1;
            {
                let x = 2;

                x
            }
        }
        "#;

        let module = syntax::parse_module(src).unwrap();

        let result = analyze_module_variable_usage(&module);

        assert_eq!(result, Ok(()));
    }

    #[test]
    fn test_prelude_identifiers() {
        let src = r#"
        effect fn foo(thing: i32) {
            let x = 1;

            print(x);
            print(thing)
        }
        "#;

        let module = syntax::parse_module(src).unwrap();

        let result = analyze_module_variable_usage(&module);

        assert_eq!(result, Ok(()));
    }

    #[test]
    fn test_while() {
        let src = r#"
        effect fn foo() {
            let x = 1;

            while (x) {
                z;
            }

            z;
        }
        "#;

        let module = syntax::parse_module(src).unwrap();

        let result = analyze_module_variable_usage(&module);

        assert_eq!(
            result,
            Err(vec![
                VariableUsageDiagnostic::UnknownIdentifier("z".to_string()),
                VariableUsageDiagnostic::UnknownIdentifier("z".to_string()),
            ])
        );
    }

    #[test]
    fn test_module_cross_reference() {
        let src = r#"
        effect fn foo() {
            foo();
            bar();
        }

        effect fn bar() {
            foo();
            bar();
            jooj();
        }
        "#;

        let module = syntax::parse_module(src).unwrap();

        let result = analyze_module_variable_usage(&module);

        assert_eq!(
            result,
            Err(vec![VariableUsageDiagnostic::UnknownIdentifier(
                "jooj".to_string()
            ),])
        );
    }

    #[test]
    fn test_expression_works() {
        let src = r#"{ let x = 10; println(x); y }"#;

        let expression = syntax::parse_expression(src).unwrap();

        let result = analyze_expression_variable_usage(&expression);

        assert_eq!(
            result,
            Err(vec![VariableUsageDiagnostic::UnknownIdentifier(
                "y".to_string()
            ),])
        );
    }
}
