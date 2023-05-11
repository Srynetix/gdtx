use serde::{Deserialize, Serialize};

use super::{common::IntType, float::FloatRepr, string::QuoteMode};

/// Value.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Value<'t> {
    Boolean(bool),
    String(&'t str, QuoteMode),
    Float(FloatRepr<'t>),
    Int(IntType),
}
