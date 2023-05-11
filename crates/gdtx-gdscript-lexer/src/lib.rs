//! GDScript lexer module.

#![deny(missing_docs)]

mod debug;
mod error;
mod lexer;
mod read_context;
mod readers;
mod serializer;
mod span;
mod token;

#[cfg(test)]
mod tests;

pub use debug::CompactTokenView;
pub use error::{Error, Result};
pub use lexer::{GdScriptLexer, GdScriptLexerOutput};
pub use read_context::TokenReaderContext;
pub use serializer::{GdScriptLexerOutputFormat, GdScriptLexerOutputSerializer};
pub use token::{
    FloatType, IndentationType, IntType, Keyword, NewLine, Operator, Punct, QuoteMode, Token, Value,
};
