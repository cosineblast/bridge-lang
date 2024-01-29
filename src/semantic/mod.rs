
use crate::syntax;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum SemanticError {
    #[error("This identifier is unknown")]
    UnknownIdentifier(String),
}

type Set<T> = std::collections::HashSet<T>;
type Map<K, V> = std::collections::HashMap<K, V>;

fn get_prelude_symbols() -> Set<String> {
    ["print", "println"]
        .map(|it| it.to_string())
            .into_iter()
            .collect()
}

struct SymbolDefinitionCheckState {
    global_symbols: Set<String>,
    local_occurences: Map<String, u32>
}

impl SymbolDefinitionCheckState {
    fn init() -> SymbolDefinitionCheckState {
        SymbolDefinitionCheckState {
            global_symbols: get_prelude_symbols(),
            local_occurences: Map::default()
        }
    }

    fn add_occurence(&mut self, name: &str) {
        self.local_occurences
            .entry(name.to_owned())
            .and_modify(|count| *count += 1)
            .or_insert(1);
    }

    fn remove_occurence(&mut self, name: &str) {
        let value = self.local_occurences.get_mut(name).expect("Tried to remove unknown identifier");

        if *value == 1 {
            self.local_occurences.remove(name);
        } else {
            *value -= 1;
        }
    }


    fn check_expression(&mut self, expression: &syntax::Expression) -> Result<(), SemanticError> {

        match expression {
            syntax::Expression::Literal(_) => {
                // no problem here officer
                // ~~unless we add string interpolation~~
            },
            syntax::Expression::If(if_expression) => {
                self.check_expression(&if_expression.condition)?;
                self.check_block(&if_expression.then_branch)?;

                if let Some(else_branch) = &if_expression.else_branch {
                    self.check_block(else_branch)?;
                }
            },
            syntax::Expression::While(while_expression) => {
                self.check_expression(&while_expression.condition)?;
                self.check_block(&while_expression.body)?;
            },
            syntax::Expression::Block(block) => {
                self.check_block(&block.0)?;
            },
            syntax::Expression::FunctionCall(call) => {
                // TODO: handle function call expression whose functions
                // aren't identifiers

                for argument in call.arguments.iter() {
                    self.check_expression(argument)?;
                }
            },
            syntax::Expression::Identifier(identifier) => {
                if !self.local_occurences.contains_key(&identifier.0) && !self.global_symbols.contains(&identifier.0) {
                    return Err(SemanticError::UnknownIdentifier(identifier.0.clone()));
                }
            },
        }


        Ok(())
    }

    fn check_block(&mut self, block: &syntax::Block) -> Result<(), SemanticError> {
        let mut block_declarations = Set::default();

        for statement in block.statements.iter() {
            match statement {
                syntax::Statement::Expression(expression) => {
                    self.check_expression(expression)?;
                },
                syntax::Statement::Let(declaration) => {
                    self.check_expression(&declaration.value)?;

                    if block_declarations.insert(declaration.name.clone()) {
                        self.add_occurence(&declaration.name);
                    }
                }
            }
        }

        for declaration in block_declarations.iter() {
            self.remove_occurence(declaration);
        }

        Ok(())
    }

    fn check_function_declaration(&mut self, function_declaration: &syntax::FunctionDeclaration) -> Result<(), SemanticError> {
        // TODO: add module identifiers to global_symbols

        for parameter in function_declaration.parameters.iter() {
            self.add_occurence(&parameter.0.symbol);
        }

        self.check_block(&function_declaration.body)?;

        Ok(())
    }
}


pub fn analyze_variable_usage(module: &syntax::Module) -> Result<(), SemanticError> {

    let mut state = SymbolDefinitionCheckState::init();

    for top_level in module.declarations.iter() {

        match top_level {

            syntax::TopLevelDeclaration::Function(function_declaration) => {

                state.check_function_declaration(function_declaration)?
            }

        }
    }

    Ok(())
}

#[cfg(test)]
mod test {

    use crate::syntax;
    use super::*;


    #[test]
    fn test_simple_usage() {
        let src = r#"
        effect fn foo() {
            let x = 1;
            if (x) { x } { y };
        }
        "#;

        let module = syntax::parse_module(src).unwrap();

        let result = analyze_variable_usage(&module);

        assert_eq!(result, Err(SemanticError::UnknownIdentifier("y".to_string())));
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

        let result = analyze_variable_usage(&module);

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

        let result = analyze_variable_usage(&module);

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
        }
        "#;

        let module = syntax::parse_module(src).unwrap();

        let result = analyze_variable_usage(&module);

        assert_eq!(result, Err(SemanticError::UnknownIdentifier("z".to_string())));
    }

}
