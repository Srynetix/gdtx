use serde::{Deserialize, Serialize};

use super::{common::IntType, float::FloatRepr, string::QuoteMode};

/// Value.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Value {
    Boolean(bool),
    String(String, QuoteMode),
    Float(FloatRepr),
    Int(IntType),
}
