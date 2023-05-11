use serde::{Deserialize, Serialize};
use std::io::{Error, ErrorKind, Write};

use crate::parser::GdClass;

/// GDScript class serializer.
#[derive(Default)]
pub struct GdClassSerializer;

/// GDScript class format.
#[derive(Serialize, Deserialize)]
pub struct GdClassFormat<'t> {
    /// Version.
    pub version: String,
    /// Class.
    #[serde(borrow)]
    pub class: GdClass<'t>,
}

impl GdClassSerializer {
    /// Serialize data.
    pub fn serialize<W: Write>(&self, data: GdClassFormat, writer: W) -> Result<(), Error> {
        serde_json::to_writer_pretty(writer, &data).map_err(|e| Error::new(ErrorKind::Other, e))
    }

    /// Deserialize data.
    pub fn deserialize<'t>(&self, value: &'t str) -> Result<GdClassFormat<'t>, Error> {
        serde_json::from_str(value).map_err(|e| Error::new(ErrorKind::Other, e))
    }
}
