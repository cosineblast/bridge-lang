use anyhow::anyhow;
use derive_more::TryInto;
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

/// A unique ID for an AST node.
/// At the current moment, we often
/// need to store additional, complex, information in the AST.
/// Such as the type of each expression, and such.
/// One approach would be to make expressions generic, but right now,
/// I feel it is just simplier to store these things in a separate table,
/// and use the ID to reference them.
///
/// We should also probably have a Node-Header thing
/// We could perhaps use a custom-derive macro.
#[derive(Debug, PartialEq, Serialize, Deserialize, Eq, Hash, Clone, Copy)]
pub struct AstId(u64);

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Module {
    pub id: AstId,
    pub span: IndexSpan,
    pub declarations: Vec<TopLevelDeclaration>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum TopLevelDeclaration {
    Function(FunctionDeclaration),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FunctionDeclaration {
    pub id: AstId,
    pub span: IndexSpan,
    pub name: Identifier,
    pub parameters: Vec<(Identifier, Type)>,
    pub return_type: Option<Type>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Block {
    pub id: AstId,
    pub span: IndexSpan,
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Identifier {
    pub id: AstId,
    pub span: IndexSpan,
    pub symbol: String,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Type {
    pub id: AstId,
    pub span: IndexSpan,
    pub name: String,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, TryInto)]
#[try_into(owned, ref, ref_mut)]
pub enum Statement {
    Let(LetStatement),
    Expression(Expression),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct LetStatement {
    pub id: AstId,
    pub span: IndexSpan,
    pub name: Identifier,
    pub type_specifier: Option<Type>,
    pub expression: Box<Expression>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, TryInto)]
#[try_into(owned, ref, ref_mut)]
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
    pub id: AstId,
    pub span: IndexSpan,
    pub condition: Box<Expression>,
    pub then_branch: Block,
    pub else_branch: Option<Block>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct LiteralExpression {
    pub id: AstId,
    pub span: IndexSpan,
    pub literal: Literal,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Integer(i64),
    String(String),
    Bool(bool),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct WhileExpression {
    pub id: AstId,
    pub span: IndexSpan,
    pub condition: Box<Expression>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct BlockExpression(pub Block);

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct IdentifierExpression(pub Identifier);

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FunctionCallExpression {
    pub id: AstId,
    pub span: IndexSpan,
    pub name: Identifier,
    pub arguments: Vec<Expression>,
}

impl AstId {
    fn new_random() -> Self {
        AstId(rand::random())
    }
}

impl TryInto<Block> for Expression {
    type Error = &'static str;

    fn try_into(self) -> Result<Block, Self::Error> {
        match self {
            Expression::Block(BlockExpression(block)) => Ok(block),
            _ => Err("Cannot convert this expression to a block"),
        }
    }
}

impl Expression {
    pub fn as_block(&self) -> Option<&Block> {
        match self {
            Expression::Block(BlockExpression(block)) => Some(block),
            _ => None,
        }
    }

    pub fn id(&self) -> AstId {
        // TODO: Refactor type (literally, like, matematically)
        // We currently have (AstId * x1) + (AstId * x2) + (AstId * x3) + ...
        // but this operation would be easier if we had
        // AstId * (x1 + x2 + x3 + ...) instead.
        // However, this would make it difficult to check
        // the ID of a specialized expression
        match self {
            Expression::If(x) => x.id,
            Expression::Literal(x) => x.id,
            Expression::While(x) => x.id,
            Expression::Block(x) => x.0.id,
            Expression::FunctionCall(x) => x.id,
            Expression::Identifier(x) => x.0.id,
        }
    }
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
            id: AstId::new_random(),
            span: IndexSpan::from(&pair.as_span()),
            symbol: pair.as_str().to_string(),
        }
    }
}

impl From<&Pair<'_, Rule>> for Type {
    fn from(pair: &Pair<'_, Rule>) -> Self {
        let identifier = Identifier::from(pair);
        Type {
            id: AstId::new_random(),
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

// This function is called parse_expr instead of parse_expression,
// because it is an internal version of parse_expression, which already exists.
fn parse_expr(source: Pair<Rule>) -> anyhow::Result<Expression> {
    let expression = match source.as_rule() {
        Rule::if_expression => {
            let span = source.as_span();
            let mut source = source.into_inner();

            let if_expression = IfExpression {
                id: AstId::new_random(),
                span: IndexSpan::from(&span),
                condition: Box::new(parse_expr(source.next().unwrap())?),
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
            let pair = source.next().unwrap();
            let literal = match pair.as_rule() {
                Rule::integer_literal => Literal::Integer(pair.as_str().parse().unwrap()),
                Rule::string_literal => Literal::String(handle_string_literal(pair.as_str())?),
                Rule::bool_literal => {
                    let value = match pair.as_str() {
                        "true" => true,
                        "false" => false,
                        _ => unreachable!(),
                    };

                    Literal::Bool(value)
                }

                _ => panic!("Unexpected rule: {:?}", pair.as_rule()),
            };

            Expression::Literal(LiteralExpression {
                id: AstId::new_random(),
                span: IndexSpan::from(&pair.as_span()),
                literal,
            })
        }
        Rule::while_expression => {
            let span = IndexSpan::from(&source.as_span());
            let mut source = source.into_inner();
            let id = AstId::new_random();
            let condition = Box::new(parse_expr(source.next().unwrap())?);
            let body = parse_block(source.next().unwrap().into_inner().next().unwrap())?;
            Expression::While(WhileExpression {
                id,
                span,
                condition,
                body,
            })
        }
        Rule::function_call => {
            let span = IndexSpan::from(&source.as_span());
            let mut stuff = source.into_inner();
            let name = Identifier::from(&stuff.next().unwrap());

            let mut arguments = Vec::new();

            if let Some(item) = stuff.next() {
                for it in item.into_inner() {
                    arguments.push(parse_expr(it)?);
                }
            }

            Expression::FunctionCall(FunctionCallExpression {
                id: AstId::new_random(),
                span,
                name,
                arguments,
            })
        }
        Rule::block => Expression::Block(BlockExpression(parse_block(source)?)),
        Rule::identifier => Expression::Identifier(IdentifierExpression(Identifier::from(&source))),
        _ => panic!("Unexpected rule: {:?}", source.as_rule()),
    };

    Ok(expression)
}

fn parse_let_statement(source: Pair<Rule>) -> anyhow::Result<LetStatement> {
    assert_eq!(source.as_rule(), Rule::let_statement);

    let span = IndexSpan::from(&source.as_span());
    let mut stuff = source.into_inner();

    let name = Identifier::from(&stuff.next().unwrap());

    let thing = stuff.peek().unwrap();

    let type_specifier = if thing.as_rule() == Rule::type_specifier {
        stuff.next();
        let pair = thing.into_inner().next().unwrap();
        let name = pair.as_str().to_string();
        let span = IndexSpan::from(&pair.as_span());
        Some(Type {
            id: AstId::new_random(),
            name,
            span,
        })
    } else {
        None
    };

    let value = Box::new(parse_expr(stuff.next().unwrap())?);

    let result = LetStatement {
        id: AstId::new_random(),
        span,
        name,
        type_specifier,
        expression: value,
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
                statements.push(Statement::Expression(parse_expr(
                    node.into_inner().next().unwrap(),
                )?));
            }

            Rule::expression_statement => {
                statements.push(Statement::Expression(parse_expr(
                    node.into_inner().next().unwrap(),
                )?));
            }

            _ => panic!("Unexpected rule: {:?}", node.as_rule()),
        }
    }

    Ok(Block {
        id: AstId::new_random(),
        span,
        statements,
    })
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

    Ok(FunctionDeclaration {
        id: AstId::new_random(),
        span,
        name,
        parameters,
        return_type,
        body,
    })
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

    Ok(Module {
        id: AstId::new_random(),
        span,
        declarations,
    })
}

pub fn parse_expression(source: &str) -> anyhow::Result<Expression> {
    let source = source.trim();

    let content = PestParser::parse(Rule::expression_file, source)?
        .next()
        .unwrap()
        .into_inner()
        .next()
        .unwrap();

    parse_expr(content)
}

// TODO: add tests for span
// right now, the insta tests don't show
// the span, so they are currently untested
// (e.g we can't know if the span really points to the right string
//    just from the snapshot)
//  alternatively, we can store the source span string in the AST.
//  We could just store the &str, but this would make many things
//  much more complicated, as the AST would need to store a reference to the source
//  string itself, making it harder to serialize, deserialize, and move around.
//
//  Another option, would be to use some sort of java-like immutable strings.
#[cfg(test)]
mod test {

    use super::*;

    macro_rules! assert_ast_snapshot {
        ($ast:expr) => {

            insta::assert_yaml_snapshot!($ast, {
                ".**.span" => "[span]",
                ".**.id" => "[id]"
            });
        };
    }

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

        assert_ast_snapshot!(module);
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

        assert_ast_snapshot!(module);
    }

    #[test]
    fn parses_expressions() {
        let source = r#"  { let x = 1; if (x) { 10 } { foo() } }  "#;

        let result = parse_expression(source).unwrap();

        assert_ast_snapshot!(result);
    }

    #[test]
    fn rejects_leading_characters() {
        let expression_source = r#"{ let x = 1; if (x) { 10 } { foo() } } jaaj"#;
        let expression = parse_expression(expression_source);

        assert!(expression.is_err());

        let module_source = r#"fn foo() { 10 } bar"#;

        let module = parse_expression(module_source);
        assert!(module.is_err());
    }
}
