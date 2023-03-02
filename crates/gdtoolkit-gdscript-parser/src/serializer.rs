use serde::{Deserialize, Serialize};
use std::io::{Error, ErrorKind, Read, Write};

use crate::parser::GdClass;

/// GDScript class serializer.
#[derive(Default)]
pub struct GdClassSerializer;

/// GDScript class format.
#[derive(Serialize, Deserialize)]
pub struct GdClassFormat {
    /// Version.
    pub version: String,
    /// Class.
    pub class: GdClass,
}

impl GdClassSerializer {
    /// Serialize data.
    pub fn serialize<W: Write>(&self, data: GdClassFormat, writer: W) -> Result<(), Error> {
        serde_json::to_writer_pretty(writer, &data).map_err(|e| Error::new(ErrorKind::Other, e))
    }

    /// Deserialize data.
    pub fn deserialize<R: Read>(&self, reader: R) -> Result<GdClassFormat, Error> {
        serde_json::from_reader(reader).map_err(|e| Error::new(ErrorKind::Other, e))
    }
}
