use serde::{Deserialize, Serialize};

use super::common::FloatType;

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct FloatRepr(String);

impl From<String> for FloatRepr {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl<'a> From<&'a str> for FloatRepr {
    fn from(value: &'a str) -> Self {
        Self(value.into())
    }
}

impl std::fmt::Display for FloatRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl FloatRepr {
    pub fn to_float(&self) -> FloatType {
        self.0.parse().unwrap()
    }
}
