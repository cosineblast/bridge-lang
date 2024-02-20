// TODO: remove pub from these modules after
// moving them to separate crates, or after using them
// in the main.rs file.

// this is just so the compiler stops complaining about unused functions.
pub mod semantic;
pub mod syntax;
pub mod codegen;
pub mod common;

struct Repl {
    codegen: codegen::Codegen,
}

impl Repl {
    fn new() -> anyhow::Result<Self> {
        Ok(Self {
            codegen: codegen::Codegen::new()?,
        })
    }

    fn start() -> anyhow::Result<()> {
        let mut repl = Repl::new()?;

        repl.run()?;

        Ok(())
    }

    fn run(&mut self) -> anyhow::Result<()> {

        let mut rl = rustyline::DefaultEditor::new()?;

        loop {
            let line = rl.readline(">> ")?;

            if line == "exit" {
                break;
            }

            rl.add_history_entry(&line)?;

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

            let type_output =
                match semantic::perform_type_check_with_preable(&expression, &semantic::PRELUDE) {
                    Err(diagnostics) => {
                        for diagnostic in diagnostics {
                            eprintln!("{}", diagnostic);
                        }

                        continue;
                    }

                    Ok(type_table) => type_table,
                };

            println!("it :: {}", type_output.type_assignments[&expression.id()]);

            let codegen_output = self.codegen.codegen_expression(&expression, type_output)?;

            println!("{}", codegen_output);
        }

        Ok(())
    }
}

fn main() -> anyhow::Result<()> {
    Repl::start()
}
