
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "syntax.pest"]
struct PestParser;

#[derive(Debug, PartialEq)]
pub struct Module {
    declarations: Vec<TopLevelDeclaration>
}

#[derive(Debug, PartialEq)]
pub enum TopLevelDeclaration {
    Function(FunctionDeclaration)
}

#[derive(Debug, PartialEq)]
pub struct FunctionDeclaration {
    name: Identifier,
    parameters: Vec<(Identifier, Type)>,
    return_type: Option<Type>,
    body: Block
}

pub type Block = Vec<Statement>;
type Identifier = String;
type Type = String;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Expression(Expression)
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    name: String,
    type_specifier: Option<Type>,
    value: Box<Expression>
}
#[derive(Debug, PartialEq)]
pub enum Expression {
    If(IfExpression),
    Literal(LiteralExpression),
    While(WhileExpression),
    Block(BlockExpression),
    FunctionCall(FunctionCallExpression),
    Identifier(IdentifierExpression),
}

#[derive(Debug, PartialEq)]
pub struct IfExpression {
    condition: Box<Expression>,
    then: Block,
    else_branch: Option<Block>
}

#[derive(Debug, PartialEq)]
pub enum LiteralExpression {
    Integer(i64),
    String(String)
}

#[derive(Debug, PartialEq)]
pub struct WhileExpression {
    condition: Box<Expression>,
    body: Block
}

#[derive(Debug, PartialEq)]
pub struct BlockExpression(Block);

#[derive(Debug, PartialEq)]
pub struct IdentifierExpression(String);

#[derive(Debug, PartialEq)]
pub struct FunctionCallExpression {
    name: String,
    arguments: Vec<Expression>
}


pub fn parse_expression(source: Pair<Rule>) -> Expression {
    match source.as_rule() {
        Rule::if_expression =>  {
            let mut source = source.into_inner();

            let if_expression = IfExpression {
                condition: Box::new(parse_expression(source.next().unwrap())),
                then: parse_block(source.next().unwrap().into_inner().next().unwrap()),
                else_branch: source.next().map(|it| parse_block(it.into_inner().next().unwrap()))
            };

            Expression::If(if_expression)
        },
        Rule::literal => {
            let mut source = source.into_inner();
            let literal = source.next().unwrap();
            match literal.as_rule() {
                Rule::integer_literal => Expression::Literal(LiteralExpression::Integer(literal.as_str().parse().unwrap())),
                Rule::string_literal => Expression::Literal(LiteralExpression::String(literal.as_str().to_string())),
                _ => panic!("Unexpected rule: {:?}", literal.as_rule())
            }
        },
        Rule::while_expression => {
            let mut source = source.into_inner();
            let condition = Box::new(parse_expression(source.next().unwrap()));
            let body = parse_block(source.next().unwrap().into_inner().next().unwrap());
            Expression::While(WhileExpression {
                condition,
                body
            })
        },
        Rule::function_call => {
            let mut stuff = source.into_inner();
            let name = stuff.next().unwrap().as_str().to_string();
            
            let arguments: Vec<Expression> = stuff.next().unwrap().into_inner().map(|node| parse_expression(node)).collect();
            
            Expression::FunctionCall(FunctionCallExpression {
                name,
                arguments
            })
            
        },
        Rule::block => {
            Expression::Block(BlockExpression(parse_block(source)))
        },
        Rule::identifier => {
            Expression::Identifier(IdentifierExpression(source.as_str().to_string()))
        },
        _ => panic!("Unexpected rule: {:?}", source.as_rule())
    }
}

pub fn parse_let_statement(source: Pair<Rule>) -> LetStatement {
    assert_eq!(source.as_rule(), Rule::let_statement);

    let mut stuff = source.into_inner();

    let name = stuff.next().unwrap().as_str().to_string();
    
    let thing = stuff.peek().unwrap();
    
    let type_specifier =  if thing.as_rule() == Rule::type_specifier {
        stuff.next();
        Some(thing.into_inner().next().unwrap().as_str().to_string())
    } else {
        None
    };
    
    let value = Box::new(parse_expression(stuff.next().unwrap()));
    
    LetStatement {
        name,
        type_specifier,
        value
    }
}

pub fn parse_block(source: Pair<Rule>) -> Block {
    assert_eq!(source.as_rule(), Rule::block);
    
    let mut statements = Vec::new();
    
    for node in source.into_inner() {
        match node.as_rule() {
            Rule::let_statement => {
                statements.push(Statement::Let(parse_let_statement(node)));
            },

            Rule::block_statement => {
                statements.push(Statement::Expression(parse_expression(node.into_inner().next().unwrap())));
            },

            Rule::expression_statement => {
                statements.push(Statement::Expression(parse_expression(node.into_inner().next().unwrap())));
            }
            
            _ => panic!("Unexpected rule: {:?}", node.as_rule())
            }
        }
    
    statements
}


pub fn parse_function_declaration(source: Pair<Rule>) -> FunctionDeclaration {
    
    let mut stuff = source.into_inner();
    
    let name = stuff.next().unwrap().as_str().to_string();
    
    let parameters = stuff.next().unwrap().into_inner().map(|node| {
        let mut children = node.into_inner();
        let name = children.next().unwrap().as_str().to_string();
        let type_ = children.next().unwrap().as_str().to_string();
        (name, type_)
    }).collect();
    
    let return_type = stuff.next().unwrap().into_inner().next().map(|node| node.as_str().to_string());
    
    let body = parse_block(stuff.next().unwrap().into_inner().next().unwrap());
    
    return FunctionDeclaration {
        name,
        parameters,
        return_type,
        body
    };
}

pub fn parse_module(source: &str) -> anyhow::Result<Module> {
    
    let file = PestParser::parse(Rule::module_file, source)?.next().unwrap();

    assert_eq!(file.as_rule(), Rule::module_file);
    
    let tree = file.into_inner().next().unwrap();

    assert_eq!(tree.as_rule(), Rule::module);
    
    let mut declarations = Vec::new();
    
    for node in tree.into_inner() {
        let function_declaration = parse_function_declaration(node);
        
        declarations.push(TopLevelDeclaration::Function(function_declaration));
    }
    
    Ok(Module {
        declarations
    })
}

#[cfg(test)]
mod test {
    
    use super::*;

    #[test]
    fn parses_simple_function() {
        let source = r#"
            effect fn main() {
            }
        "#;

        let module = parse_module(source).unwrap();
        
        assert_eq!(module.declarations.len(), 1);
        
        match &module.declarations[0] {
            TopLevelDeclaration::Function(function) => {
                assert_eq!(function.name, "main");
                assert_eq!(function.parameters.len(), 0);
                assert_eq!(function.return_type, None);
                assert_eq!(function.body.len(), 0);
            }
        }
    }
    
    #[test]
    fn parses_complex_input() {
        let source = r#"
effect fn foo(x: i32, y: u32) -> u32 {
    let x: i32 = 10;
    let y = 20;

    if (food) { 30 }
    bye
}
        "#;
        
        let module = parse_module(source).unwrap();

        assert_eq!(module.declarations.len(), 1);
        
        // TODO: find something like nubank's matchers combinators

        match &module.declarations[0] {
            TopLevelDeclaration::Function(function) => {
                assert_eq!(function.name, "foo");
                assert_eq!(function.parameters.len(), 2);
                assert_eq!(function.return_type, Some("u32".to_string()));
                assert_eq!(function.body.len(), 4);
                
                match &function.body[0] {
                    Statement::Let(let_statement) => {
                        assert_eq!(let_statement.name, "x");
                        assert_eq!(let_statement.type_specifier, Some("i32".to_string()));
                        assert_eq!(let_statement.value, Box::new(Expression::Literal(LiteralExpression::Integer(10))));
                    }
                    _ => panic!("Unexpected statement")
                }
                
                match &function.body[1] {
                    Statement::Let(let_statement) => {
                        assert_eq!(let_statement.name, "y");
                        assert_eq!(let_statement.type_specifier, None);
                        assert_eq!(let_statement.value, Box::new(Expression::Literal(LiteralExpression::Integer(20))));
                    }
                    _ => panic!("Unexpected statement")
                }
                
                match &function.body[2] {
                    Statement::Expression(Expression::If(if_expression)) => {
                        assert_eq!(if_expression.condition, Box::new(Expression::Identifier(IdentifierExpression("food".to_string()))));
                        assert_eq!(if_expression.then.len(), 1);
                        assert_eq!(if_expression.else_branch, None);
                        
                        match &if_expression.then[0] {
                            Statement::Expression(Expression::Literal(LiteralExpression::Integer(30))) => {},
                            _ => panic!("Unexpected statement")
                        }
                    }
                    _ => panic!("Unexpected expression")
                };
                
                match &function.body[3] {
                    Statement::Expression(Expression::Identifier(IdentifierExpression(string))) => {
                        assert_eq!(string, "bye");
                    },
                    _ => panic!("Unexpected statement")
                }
            },
        }
    }
    
}