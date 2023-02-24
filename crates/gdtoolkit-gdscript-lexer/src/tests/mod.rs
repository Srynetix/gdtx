use crate::token::QuoteMode;

use super::*;
use pretty_assertions::assert_eq;

fn tok_int(value: IntType) -> Token {
    Token::Value(Value::Int(value))
}

fn tok_bool(value: bool) -> Token {
    Token::Value(Value::Boolean(value))
}

fn tok_dqstr(value: &str) -> Token {
    Token::Value(Value::String(value.into(), QuoteMode::Double))
}

fn tok_kw(value: Keyword) -> Token {
    Token::Keyword(value)
}

fn tok_ws(value: &str) -> Token {
    Token::Whitespace(value.into())
}

fn tok_op(value: Operator) -> Token {
    Token::Operator(value)
}

fn tok_ident(value: &str) -> Token {
    Token::Ident(value.into())
}

fn tok_indent(size: usize) -> Token {
    Token::Indent(size)
}

fn tok_punct(value: Punct) -> Token {
    Token::Punct(value)
}

fn lex_tokens(text: &str) -> Vec<Token> {
    let output = GdScriptLexer::default().lex(text).unwrap();
    output.tokens()
}

#[test]
fn test_expr_1() {
    assert_eq!(
        lex_tokens("1 += 1abcd + (a * 2) && 5 & 1"),
        vec![
            tok_int(1),
            tok_ws(" "),
            tok_op(Operator::AddAssign),
            tok_ws(" "),
            tok_int(1),
            tok_ident("abcd"),
            tok_ws(" "),
            tok_punct(Punct::Plus),
            tok_ws(" "),
            tok_punct(Punct::OpenParens),
            tok_ident("a"),
            tok_ws(" "),
            tok_punct(Punct::Asterisk),
            tok_ws(" "),
            tok_int(2),
            tok_punct(Punct::ClosedParens),
            tok_ws(" "),
            tok_op(Operator::BinAnd),
            tok_ws(" "),
            tok_int(5),
            tok_ws(" "),
            tok_punct(Punct::Ampersand),
            tok_ws(" "),
            tok_int(1),
            Token::Eof
        ]
    )
}

#[test]
fn test_bool() {
    assert_eq!(
        lex_tokens("pouet = true and false || true"),
        vec![
            tok_ident("pouet"),
            tok_ws(" "),
            tok_punct(Punct::Eq),
            tok_ws(" "),
            tok_bool(true),
            tok_ws(" "),
            tok_kw(Keyword::And),
            tok_ws(" "),
            tok_bool(false),
            tok_ws(" "),
            tok_op(Operator::BinOr),
            tok_ws(" "),
            tok_bool(true),
            Token::Eof
        ]
    )
}

#[test]
fn test_func() {
    assert_eq!(
        lex_tokens("static func dummy(a: int) -> void:\n    pass"),
        vec![
            tok_kw(Keyword::Static),
            tok_ws(" "),
            tok_kw(Keyword::Func),
            tok_ws(" "),
            tok_ident("dummy"),
            tok_punct(Punct::OpenParens),
            tok_ident("a"),
            tok_punct(Punct::Colon),
            tok_ws(" "),
            tok_ident("int"),
            tok_punct(Punct::ClosedParens),
            tok_ws(" "),
            tok_op(Operator::TypeArrow),
            tok_ws(" "),
            tok_ident("void"),
            tok_punct(Punct::Colon),
            Token::LineFeed,
            tok_indent(1),
            tok_kw(Keyword::Pass),
            Token::Eof
        ]
    )
}

#[test]
fn test_strings() {
    assert_eq!(
        lex_tokens(
            "var output = \"\"\nvar new_prefix := \"   \" if last else \" │ \"\noutput += 1"
        ),
        vec![
            tok_kw(Keyword::Var),
            tok_ws(" "),
            tok_ident("output"),
            tok_ws(" "),
            tok_punct(Punct::Eq),
            tok_ws(" "),
            tok_dqstr(""),
            Token::LineFeed,
            tok_kw(Keyword::Var),
            tok_ws(" "),
            tok_ident("new_prefix"),
            tok_ws(" "),
            tok_op(Operator::Infer),
            tok_ws(" "),
            tok_dqstr("   "),
            tok_ws(" "),
            tok_kw(Keyword::If),
            tok_ws(" "),
            tok_ident("last"),
            tok_ws(" "),
            tok_kw(Keyword::Else),
            tok_ws(" "),
            tok_dqstr(" │ "),
            Token::LineFeed,
            tok_ident("output"),
            tok_ws(" "),
            tok_op(Operator::AddAssign),
            tok_ws(" "),
            tok_int(1),
            Token::Eof
        ]
    )
}

#[test]
fn test_sample_1() {
    let res = lex_tokens(include_str!("./samples/sample01.gd"));
    assert!(!res.is_empty());

    println!("{:#?}", res);
}
