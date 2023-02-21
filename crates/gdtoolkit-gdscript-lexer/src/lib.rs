// From https://michael-f-bryan.github.io/static-analyser-in-rust/book/lex.html

use std::error::Error;

pub type IntType = i64;
pub type FloatType = f64;

type Result<T> = core::result::Result<T, Box<dyn Error + Send + Sync + 'static>>;

pub struct Lexer {
}

impl Lexer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn take_while<'a, F>(&self, data: &'a str, mut predicate: F) -> Result<(&'a str, usize)>
        where F: FnMut(char) -> bool
    {
        let mut current_index = 0;
        for ch in data.chars() {
            let should_continue = predicate(ch);

            if !should_continue {
                break;
            }

            current_index += ch.len_utf8();
        }

        if current_index == 0 {
            Err("No Matches".into())
        } else {
            Ok((&data[..current_index], current_index))
        }
    }

    pub fn read_ident(&self, data: &str) -> Result<(Token, usize)> {
        // identifiers can't start with a number
        match data.chars().next() {
            Some(ch) if ch.is_digit(10) => return Err("Identifiers can't start with a number".into()),
            None => return Err("Unexpected EOF".into()),
            _ => {},
        }

        let (got, bytes_read) = self.take_while(data, |ch| ch == '_' || ch.is_alphanumeric())?;

        // TODO: Recognise keywords using a `match` statement here.

        let tok = Token::Ident(got.to_string());
        Ok((tok, bytes_read))
    }

    pub fn lex(&self, code: &str) -> Vec<Token> {
        let chars: Vec<_> = code.chars().collect();
        let cursor = 0;
        let tokens = vec![];

        // TODO

        tokens
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Ident(String),
    Value(Value),
    Operator(Operator),
    Punct(Punct),
    NewLine,
    Whitespace(IntType),
    Eof
}

#[derive(Debug, PartialEq)]
pub enum Punct {
    Comment,
    Semicolon,
    OpenParens,
    ClosedParens,
    OpenCurly,
    ClosedCurly,
    OpenBracket,
    ClosedBracket,
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Int(IntType),
    Float(FloatType),
    String(String),
    Boolean(bool)
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Assign,
    Infer,
    Type,
    Eq,
    NotEq,
    LowerThan,
    GreaterThan,
    LowerThanEq,
    GreaterThanEq,
    UnAnd,
    UnOr,
    Not,
    And,
    Or,
    Xor
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Var,
    Export,
    Static,
    If,
    Elif,
    Else,
    While,
    For,
    OnReady,
    Switch,
    Case,
    Return,
    Break,
    Continue,
    Yield,
    Signal,
    Func,
    Class,
    Enum,
    Const
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_one() {
        let mut lexer = Lexer::new();
        assert_eq!(
            lexer.lex("1 + 1 + (a * 2)"),
            vec![
                Token::Value(Value::Int(1)),
                Token::Whitespace(1),
                Token::Operator(Operator::Plus),
                Token::Whitespace(1),
                Token::Value(Value::Int(1)),
                Token::Whitespace(1),
                Token::Operator(Operator::Plus),
                Token::Whitespace(1),
                Token::Punct(Punct::OpenParens),
                Token::Ident("a".into()),
                Token::Whitespace(1),
                Token::Operator(Operator::Multiply),
                Token::Whitespace(1),
                Token::Value(Value::Int(2)),
                Token::Punct(Punct::ClosedParens),
                Token::Eof
            ]
        )
    }
}