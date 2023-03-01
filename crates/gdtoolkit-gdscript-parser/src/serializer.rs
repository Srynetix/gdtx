use serde::{Deserialize, Serialize};
use std::io::{Error, ErrorKind, Read, Write};

use crate::parser::GdClass;

#[derive(Default)]
pub struct GdClassSerializer;

#[derive(Serialize, Deserialize)]
pub struct GdClassFormat {
    pub version: String,
    pub class: GdClass,
}

impl GdClassSerializer {
    pub fn serialize<W: Write>(&self, data: GdClassFormat, writer: W) -> Result<(), Error> {
        serde_json::to_writer_pretty(writer, &data).map_err(|e| Error::new(ErrorKind::Other, e))
    }

    pub fn deserialize<R: Read>(&self, reader: R) -> Result<GdClassFormat, Error> {
        serde_json::from_reader(reader).map_err(|e| Error::new(ErrorKind::Other, e))
    }
}
