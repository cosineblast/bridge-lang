// TODO: remove pub from these modules after
// moving them to separate crates, or after using them
// in the main.rs file.

// this is just so the compiler stops complaining about unused functions.
pub mod semantic;
pub mod syntax;

struct Repl {
}

impl Repl {
    fn start() -> anyhow::Result<()> {
        let mut rl = rustyline::DefaultEditor::new()?;

        loop {
            let line = rl.readline(">> ")?;

            if line == "exit" {
                break;
            }

            let expression = match syntax::parse_expression(&line) {
                Ok(expr) => expr,
                Err(e) => {
                    eprintln!("Error parsing expression: {}", e);
                    continue;
                }
            };

            if let Err(diagnostics) = semantic::analyze_expression_variable_usage(&expression) {
                for diagnostic in diagnostics {
                    eprintln!("{}", diagnostic);
                }
                continue;
            }

            let type_output = match semantic::perform_type_check(&expression) {
                Err(diagnostics) => {
                    for diagnostic in diagnostics {
                        eprintln!("{}", diagnostic);
                    }

                    continue;
                }

                Ok(type_table) => type_table,
            };

            println!("it :: {}", type_output.type_assignments[&expression.id()]);
        }

        Ok(())
    }
}


fn main() -> anyhow::Result<()> {
    Repl::start()
}
