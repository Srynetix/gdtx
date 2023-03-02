use serde::{Deserialize, Serialize};

pub use self::{
    common::FloatType, common::IntType, indentation::IndentationType, keyword::Keyword,
    newline::NewLine, operator::Operator, punct::Punct, string::QuoteMode, value::Value,
};

mod common;
mod float;
mod indentation;
mod keyword;
mod newline;
mod operator;
mod punct;
mod string;
mod value;

/// Token.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Token {
    Comment(String),
    Dedent,
    Eof,
    Identifier(String),
    Indent,
    Keyword(Keyword),
    NewLine(NewLine),
    NodePath(String, Option<QuoteMode>),
    Operator(Operator),
    Punct(Punct),
    Value(Value),
    Whitespace(String),
}
