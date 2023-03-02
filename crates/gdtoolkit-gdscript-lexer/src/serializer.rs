use serde::{Deserialize, Serialize};
use std::io::{Error, ErrorKind, Read, Write};

use crate::{lexer::GdScriptLexerOutput, IndentationType, Token};

/// Lexer output serializer.
#[derive(Default)]
pub struct LexerOutputSerializer;

/// Lexer output format.
#[derive(Serialize, Deserialize)]
pub struct LexerOutputFormat {
    /// Version.
    pub version: String,
    /// Indentation type.
    pub indentation_type: IndentationType,
    /// Indentation size.
    pub indentation_size: usize,
    /// Tokens.
    pub tokens: Vec<Token>,
}

impl From<&GdScriptLexerOutput> for LexerOutputFormat {
    fn from(value: &GdScriptLexerOutput) -> Self {
        let ctx = value.context();

        Self {
            version: "0.1.0".into(),
            indentation_type: ctx.indentation_type,
            indentation_size: ctx.indentation_size,
            tokens: value.tokens(),
        }
    }
}

impl LexerOutputSerializer {
    /// Serialize data.
    pub fn serialize<W: Write>(&self, data: LexerOutputFormat, writer: W) -> Result<(), Error> {
        serde_json::to_writer_pretty(writer, &data).map_err(|e| Error::new(ErrorKind::Other, e))
    }

    /// Deserialize data.
    pub fn deserialize<R: Read>(&self, reader: R) -> Result<LexerOutputFormat, Error> {
        serde_json::from_reader(reader).map_err(|e| Error::new(ErrorKind::Other, e))
    }
}
