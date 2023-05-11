//! GDScript parser module.

#![deny(missing_docs)]

mod parser;
mod serializer;

pub use parser::{GdClass, GdScriptParser};
pub use serializer::{GdClassFormat, GdClassSerializer};
