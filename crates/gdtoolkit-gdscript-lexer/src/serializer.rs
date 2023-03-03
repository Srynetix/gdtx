use serde::{Deserialize, Serialize};
use std::io::{Error, ErrorKind, Write};

use crate::{lexer::GdScriptLexerOutput, IndentationType, Token};

/// Lexer output serializer.
#[derive(Default)]
pub struct GdScriptLexerOutputSerializer;

/// Lexer output format.
#[derive(Serialize, Deserialize)]
pub struct GdScriptLexerOutputFormat<'t> {
    /// Version.
    pub version: String,
    /// Indentation type.
    pub indentation_type: IndentationType,
    /// Indentation size.
    pub indentation_size: usize,
    /// Tokens.
    #[serde(borrow)]
    pub tokens: Vec<Token<'t>>,
}

impl<'t> From<&'t GdScriptLexerOutput<'t>> for GdScriptLexerOutputFormat<'t> {
    fn from(value: &'t GdScriptLexerOutput<'t>) -> Self {
        let ctx = value.context();

        Self {
            version: "0.1.0".into(),
            indentation_type: ctx.indentation_type,
            indentation_size: ctx.indentation_size,
            tokens: value.tokens(),
        }
    }
}

impl GdScriptLexerOutputSerializer {
    /// Serialize data.
    pub fn serialize<W: Write>(
        &self,
        data: GdScriptLexerOutputFormat,
        writer: W,
    ) -> Result<(), Error> {
        serde_json::to_writer_pretty(writer, &data).map_err(|e| Error::new(ErrorKind::Other, e))
    }

    /// Deserialize data.
    pub fn deserialize<'t>(&self, value: &'t str) -> Result<GdScriptLexerOutputFormat<'t>, Error> {
        serde_json::from_str(value).map_err(|e| Error::new(ErrorKind::Other, e))
    }
}
