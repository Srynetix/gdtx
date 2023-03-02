use crate::token::{NewLine, QuoteMode};

use super::*;
use pretty_assertions::assert_eq;

fn tok_int<'t>(value: IntType) -> Token<'t> {
    Token::Value(Value::Int(value))
}

fn tok_bool<'t>(value: bool) -> Token<'t> {
    Token::Value(Value::Boolean(value))
}

fn tok_dqstr(value: &str) -> Token {
    Token::Value(Value::String(value, QuoteMode::Double))
}

fn tok_kw<'t>(value: Keyword) -> Token<'t> {
    Token::Keyword(value)
}

fn tok_ws(value: &str) -> Token {
    Token::Whitespace(value)
}

fn tok_op<'t>(value: Operator) -> Token<'t> {
    Token::Operator(value)
}

fn tok_ident(value: &str) -> Token {
    Token::Identifier(value)
}

fn tok_punct<'t>(value: Punct) -> Token<'t> {
    Token::Punct(value)
}

fn lex_tokens(values: &str) -> Vec<Token> {
    let output = GdScriptLexer::default().lex(values).unwrap();
    output.tokens()
}

#[test]
fn expr_1() {
    assert_eq!(
        lex_tokens(concat!("1 += 1abcd + (a * 2) && 5 & 1")),
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
fn bool() {
    assert_eq!(
        lex_tokens(concat!("pouet = true and false || true")),
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
#[rustfmt::skip]
fn func() {
    assert_eq!(
        lex_tokens(concat!(
            "static func dummy(a: int) -> void:\n",
            "    pass"
        )),
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
            Token::NewLine(NewLine::Lf),
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::Eof
        ]
    )
}

#[test]
#[rustfmt::skip]
fn indents() {
    assert_eq!(
        lex_tokens(concat!(
            "if hello:\n",
            "    if hi:\n",
            "        pass\n",
            "        pass\n",
            "print('Hey!')"
        )),
        vec![
            tok_kw(Keyword::If),
            tok_ws(" "),
            tok_ident("hello"),
            tok_punct(Punct::Colon),
            Token::NewLine(NewLine::Lf),
            Token::Indent,
            tok_kw(Keyword::If),
            tok_ws(" "),
            tok_ident("hi"),
            tok_punct(Punct::Colon),
            Token::NewLine(NewLine::Lf),
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            Token::Dedent,
            Token::Dedent,
            tok_ident("print"),
            tok_punct(Punct::OpenParens),
            Token::Value(Value::String("Hey!", QuoteMode::Single)),
            tok_punct(Punct::ClosedParens),
            Token::Eof
        ]
    );

    assert_eq!(
        lex_tokens(concat!(
            "if hello:\n",
            "    if hi:\n",
            "        pass\n",
            "    pass\n",
            "    if a:\n",
            "        pass\n",
            "    pass\n",
            "pass"
        )),
        vec![
            // Line 1
            tok_kw(Keyword::If),
            tok_ws(" "),
            tok_ident("hello"),
            tok_punct(Punct::Colon),
            Token::NewLine(NewLine::Lf),
            // Line 2
            Token::Indent,
            tok_kw(Keyword::If),
            tok_ws(" "),
            tok_ident("hi"),
            tok_punct(Punct::Colon),
            Token::NewLine(NewLine::Lf),
            // Line 3
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 4
            Token::Dedent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 5
            tok_kw(Keyword::If),
            tok_ws(" "),
            tok_ident("a"),
            tok_punct(Punct::Colon),
            Token::NewLine(NewLine::Lf),
            // Line 6
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 7
            Token::Dedent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 8
            Token::Dedent,
            tok_kw(Keyword::Pass),
            Token::Eof
        ]
    );
}

#[test]
fn indents_2() {
    // Special case: multiple dedents
    assert_eq!(
        lex_tokens(concat!(
            "pass\n",
            "    pass\n",
            "        pass\n",
            "            pass\n",
            "    pass",
        )),
        vec![
            // Line 1
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 2
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 3
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 4
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 5
            Token::Dedent,
            Token::Dedent,
            tok_kw(Keyword::Pass),
            // End
            Token::Eof
        ]
    )
}

#[test]
#[rustfmt::skip]
fn indents_on_newline() {
    // Special case: multiple dedents
    assert_eq!(
        lex_tokens(concat!(
            "pass\n",
            "    pass\n",
            "    \n",
            "pass"
        )),
        vec![
            // Line 1
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 2
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 3
            Token::NewLine(NewLine::Lf),
            // Line 4
            Token::Dedent,
            tok_kw(Keyword::Pass),
            // End
            Token::Eof
        ]
    );
}

#[test]
#[rustfmt::skip]
fn indents_on_newline_2() {
    assert_eq!(
        lex_tokens(concat!(
            "pass\n",
            "    pass\n",
            "\n",
            "pass",
        )),
        vec![
            // Line 1
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 2
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 3
            Token::Dedent,
            Token::NewLine(NewLine::Lf),
            // Line 4
            tok_kw(Keyword::Pass),
            // End
            Token::Eof
        ]
    )
}

#[test]
#[rustfmt::skip]
fn indents_on_newline_3() {
    assert_eq!(
        lex_tokens(concat!(
            "pass\n",
            "    pass\n",
            "    \n",
            "    \n",
            "    pass",
        )),
        vec![
            // Line 1
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 2
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 3
            Token::NewLine(NewLine::Lf),
            // Line 4
            Token::NewLine(NewLine::Lf),
            // Line 5
            tok_kw(Keyword::Pass),
            // End
            Token::Eof
        ]
    )
}

#[test]
#[rustfmt::skip]
fn indents_on_newline_4() {
    assert_eq!(
        lex_tokens(concat!(
            "pass\n",
            "    pass\n",
            "        pass\n",
            "\n",
            "        pass",
        )),
        vec![
            // Line 1
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 2
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 3
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 4
            Token::Dedent,
            Token::Dedent,
            Token::NewLine(NewLine::Lf),
            // Line 5
            Token::Indent,
            Token::Indent,
            tok_kw(Keyword::Pass),
            // End
            Token::Eof
        ]
    )
}

#[test]
#[rustfmt::skip]
fn indents_on_newline_5() {
    assert_eq!(
        lex_tokens(concat!(
            "pass\n",
            "    pass\n",
            "        pass\n",
            "\n",
            "        pass\n",
            "    ",
        )),
        vec![
            // Line 1
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 2
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 3
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 4
            Token::Dedent,
            Token::Dedent,
            Token::NewLine(NewLine::Lf),
            // Line 5
            Token::Indent,
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 6
            Token::Dedent,
            Token::Eof
        ]
    )
}

#[test]
#[rustfmt::skip]
fn indents_on_newline_6() {
    assert_eq!(
        lex_tokens(concat!(
            "pass\n",
            "    pass\n",
            "        pass\n",
            "\n",
            "        pass\n",
            "",
        )),
        vec![
            // Line 1
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 2
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 3
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 4
            Token::Dedent,
            Token::Dedent,
            Token::NewLine(NewLine::Lf),
            // Line 5
            Token::Indent,
            Token::Indent,
            tok_kw(Keyword::Pass),
            Token::NewLine(NewLine::Lf),
            // Line 6
            Token::Dedent,
            Token::Dedent,
            Token::Eof
        ]
    )
}

#[test]
#[rustfmt::skip]
fn strings() {
    assert_eq!(
        lex_tokens(concat!(
            "var output = \"\"\n",
            "var new_prefix := \"   \" if last else \" │ \"\n",
            "output += 1",
        )),
        vec![
            tok_kw(Keyword::Var),
            tok_ws(" "),
            tok_ident("output"),
            tok_ws(" "),
            tok_punct(Punct::Eq),
            tok_ws(" "),
            tok_dqstr(""),
            Token::NewLine(NewLine::Lf),
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
            Token::NewLine(NewLine::Lf),
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
fn sample_1() {
    let res = lex_tokens(include_str!("./samples/sample01.gd"));
    assert!(!res.is_empty());

    println!("{:#?}", res);
}
