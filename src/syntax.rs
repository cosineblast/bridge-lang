use anyhow::anyhow;
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use serde::{Deserialize, Serialize};

#[derive(Parser)]
#[grammar = "syntax.pest"]
struct PestParser;

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone, Copy)]
pub struct IndexSpan {
    start: usize,
    end: usize,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Module {
    pub span: IndexSpan,
    pub declarations: Vec<TopLevelDeclaration>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum TopLevelDeclaration {
    Function(FunctionDeclaration),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FunctionDeclaration {
    pub span: IndexSpan,
    pub name: Identifier,
    pub parameters: Vec<(Identifier, Type)>,
    pub return_type: Option<Type>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Block {
    pub span: IndexSpan,
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Identifier {
    pub span: IndexSpan,
    pub symbol: String,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Type {
    pub span: IndexSpan,
    pub name: String,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    Let(LetStatement),
    Expression(Expression),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct LetStatement {
    pub name: String,
    pub type_specifier: Option<Type>,
    pub value: Box<Expression>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    If(IfExpression),
    Literal(LiteralExpression),
    While(WhileExpression),
    Block(BlockExpression),
    FunctionCall(FunctionCallExpression),
    Identifier(IdentifierExpression),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub then_branch: Block,
    pub else_branch: Option<Block>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum LiteralExpression {
    Integer(i64),
    String(String),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct WhileExpression {
    pub condition: Box<Expression>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct BlockExpression(pub Block);

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct IdentifierExpression(pub String);

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FunctionCallExpression {
    pub name: String,
    pub arguments: Vec<Expression>,
}

impl From<&pest::Span<'_>> for IndexSpan {
    fn from(span: &pest::Span<'_>) -> Self {
        IndexSpan {
            start: span.start(),
            end: span.end(),
        }
    }
}

impl From<&Pair<'_, Rule>> for Identifier {
    fn from(pair: &Pair<'_, Rule>) -> Self {
        Identifier {
            span: IndexSpan::from(&pair.as_span()),
            symbol: pair.as_str().to_string(),
        }
    }
}

impl From<&Pair<'_, Rule>> for Type {
    fn from(pair: &Pair<'_, Rule>) -> Self {
        let identifier = Identifier::from(pair);
        Type {
            span: identifier.span,
            name: identifier.symbol,
        }
    }
}

/// Our grammar ensures the valid string has correct \ escapes,
/// and ensures our string literal has " at the start and end,
/// so we better convert it to an ordinary string.
fn handle_string_literal(string: &str) -> anyhow::Result<String> {
    let mut result = String::new();

    let mut chars = string.chars();

    // skip first quote
    chars.next();

    // add everything (including ending quote)
    // TODO: extract into another function
    while let Some(c) = chars.next() {
        if c == '\\' {
            let next = chars.next().unwrap();
            match next {
                'n' => result.push('\n'),
                't' => result.push('\t'),
                '\\' => result.push('\\'),
                '"' => result.push('"'),
                'u' => {
                    let mut unicode = String::new();
                    for _ in 0..4 {
                        unicode.push(chars.next().unwrap());
                    }
                    let code_point = u32::from_str_radix(&unicode, 16).unwrap();
                    if let Some(c) = std::char::from_u32(code_point) {
                        result.push(c);
                    } else {
                        return Err(anyhow!("Invalid unicode escape sequence: \\u{}", unicode));
                    }
                }
                _ => return Err(anyhow!("Unexpected escape sequence: \\{}", next)),
            }
        } else {
            result.push(c);
        }
    }

    // skip last quote
    result.pop();

    Ok(result)
}

fn parse_expression(source: Pair<Rule>) -> anyhow::Result<Expression> {
    let expression = match source.as_rule() {
        Rule::if_expression => {
            let mut source = source.into_inner();

            let if_expression = IfExpression {
                condition: Box::new(parse_expression(source.next().unwrap())?),
                then_branch: parse_block(source.next().unwrap().into_inner().next().unwrap())?,
                else_branch: source
                    .next()
                    .map(|it| parse_block(it.into_inner().next().unwrap()))
                    .transpose()?,
            };

            Expression::If(if_expression)
        }
        Rule::literal => {
            let mut source = source.into_inner();
            let literal = source.next().unwrap();
            match literal.as_rule() {
                Rule::integer_literal => Expression::Literal(LiteralExpression::Integer(
                    literal.as_str().parse().unwrap(),
                )),
                Rule::string_literal => Expression::Literal(LiteralExpression::String(
                    handle_string_literal(literal.as_str())?,
                )),
                _ => panic!("Unexpected rule: {:?}", literal.as_rule()),
            }
        }
        Rule::while_expression => {
            let mut source = source.into_inner();
            let condition = Box::new(parse_expression(source.next().unwrap())?);
            let body = parse_block(source.next().unwrap().into_inner().next().unwrap())?;
            Expression::While(WhileExpression { condition, body })
        }
        Rule::function_call => {
            let mut stuff = source.into_inner();
            let name = stuff.next().unwrap().as_str().to_string();

            let mut arguments = Vec::new();

            if let Some(item) = stuff.next() {
                for it in item.into_inner() {
                    arguments.push(parse_expression(it)?);
                }
            }

            Expression::FunctionCall(FunctionCallExpression { name, arguments })
        }
        Rule::block => Expression::Block(BlockExpression(parse_block(source)?)),
        Rule::identifier => {
            Expression::Identifier(IdentifierExpression(source.as_str().to_string()))
        }
        _ => panic!("Unexpected rule: {:?}", source.as_rule()),
    };

    Ok(expression)
}

fn parse_let_statement(source: Pair<Rule>) -> anyhow::Result<LetStatement> {
    assert_eq!(source.as_rule(), Rule::let_statement);

    let mut stuff = source.into_inner();

    let name = stuff.next().unwrap().as_str().to_string();

    let thing = stuff.peek().unwrap();

    let type_specifier = if thing.as_rule() == Rule::type_specifier {
        stuff.next();
        let pair = thing.into_inner().next().unwrap();
        let name = pair.as_str().to_string();
        let span = IndexSpan::from(&pair.as_span());
        Some(Type { name, span })
    } else {
        None
    };

    let value = Box::new(parse_expression(stuff.next().unwrap())?);

    let result = LetStatement {
        name,
        type_specifier,
        value,
    };

    Ok(result)
}

fn parse_block(source: Pair<Rule>) -> anyhow::Result<Block> {
    assert_eq!(source.as_rule(), Rule::block);

    let mut statements = Vec::new();

    let span = IndexSpan::from(&source.as_span());

    for node in source.into_inner() {
        match node.as_rule() {
            Rule::let_statement => {
                statements.push(Statement::Let(parse_let_statement(node)?));
            }

            Rule::block_statement => {
                statements.push(Statement::Expression(parse_expression(
                    node.into_inner().next().unwrap(),
                )?));
            }

            Rule::expression_statement => {
                statements.push(Statement::Expression(parse_expression(
                    node.into_inner().next().unwrap(),
                )?));
            }

            _ => panic!("Unexpected rule: {:?}", node.as_rule()),
        }
    }

    Ok(Block { span, statements })
}

fn parse_function_declaration(source: Pair<Rule>) -> anyhow::Result<FunctionDeclaration> {
    let span = IndexSpan::from(&source.as_span());

    let mut stuff = source.into_inner();

    let name = Identifier::from(&stuff.next().unwrap());

    let parameters = stuff
        .next()
        .unwrap()
        .into_inner()
        .map(|node| {
            let mut children = node.into_inner();
            let identifier = Identifier::from(&children.next().unwrap());
            let type_value = Type::from(&children.next().unwrap());
            (identifier, type_value)
        })
        .collect();

    let return_type = stuff
        .next()
        .unwrap()
        .into_inner()
        .next()
        .map(|it| Type::from(&it));

    let body = parse_block(stuff.next().unwrap().into_inner().next().unwrap())?;

    return Ok(FunctionDeclaration {
        span,
        name,
        parameters,
        return_type,
        body,
    });
}

pub fn parse_module(source: &str) -> anyhow::Result<Module> {
    let file = PestParser::parse(Rule::module_file, source)?
        .next()
        .unwrap();

    assert_eq!(file.as_rule(), Rule::module_file);

    let tree = file.into_inner().next().unwrap();
    let span = IndexSpan::from(&tree.as_span());

    assert_eq!(tree.as_rule(), Rule::module);

    let mut declarations = Vec::new();

    for node in tree.into_inner() {
        let function_declaration = parse_function_declaration(node)?;

        declarations.push(TopLevelDeclaration::Function(function_declaration));
    }

    Ok(Module { span, declarations })
}

// TODO: add tests for span
// right now, the insta tests don't show
// the span, so they are currently untested
// (e.g we can't know if the span really points to the right string
//    just from the snapshot)
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
                assert_eq!(function.name.symbol, "main");
                assert_eq!(function.parameters.len(), 0);
                assert_eq!(function.return_type, None);
                assert_eq!(function.body.statements.len(), 0);
            }
        }
    }

    #[test]
    fn parses_literals_correctly() {
        let source = r#"
        effect fn main() {
            10;
            "cool";
            print("\"\u2211\ti = 0 to \\ n of i is n(n+1)/2\n\"");
        }"#;

        let module = parse_module(source).unwrap();

        insta::assert_yaml_snapshot!(module, {
            ".**.span" => "[span]"
        });
    }

    #[test]
    fn parses_complex_input() {
        let source = r#"
effect fn foo(x: i32, y: u32) -> u32 {
    let x: i32 = 10;
    let y = 20;

    if (food) { 30 }
    ls(1);
    bye()
}
        "#;

        let module = parse_module(source).unwrap();

        insta::assert_yaml_snapshot!(module, {
            ".**.span" => "[span]"
        });
    }
}
