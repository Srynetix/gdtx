//! GDScript lexer module.

#![deny(missing_docs)]

mod error;
mod lexer;
mod read;
mod serializer;
mod token;

#[cfg(test)]
mod tests;

pub use error::{Error, Result};
pub use lexer::{GdScriptLexer, GdScriptLexerOutput};
pub use read::TokenReaderContext;
pub use serializer::{LexerOutputFormat, LexerOutputSerializer};
pub use token::{
    CompactTokenView, FloatType, IndentationType, IntType, Keyword, NewLine, Operator, Punct,
    QuoteMode, Token, Value,
};
