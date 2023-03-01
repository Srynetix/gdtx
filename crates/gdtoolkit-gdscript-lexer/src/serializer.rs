use serde::{Deserialize, Serialize};
use std::io::{Error, ErrorKind, Read, Write};

use crate::{lexer::GdScriptLexerOutput, IndentationType, Token};

#[derive(Default)]
pub struct LexerOutputSerializer;

#[derive(Serialize, Deserialize)]
pub struct LexerOutputFormat {
    pub version: String,
    pub indentation_type: IndentationType,
    pub indentation_size: usize,
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
    pub fn serialize<W: Write>(&self, data: LexerOutputFormat, writer: W) -> Result<(), Error> {
        serde_json::to_writer_pretty(writer, &data).map_err(|e| Error::new(ErrorKind::Other, e))
    }

    pub fn deserialize<R: Read>(&self, reader: R) -> Result<LexerOutputFormat, Error> {
        serde_json::from_reader(reader).map_err(|e| Error::new(ErrorKind::Other, e))
    }
}
