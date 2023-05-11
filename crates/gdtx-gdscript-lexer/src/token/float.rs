use serde::{Deserialize, Serialize};

use super::common::FloatType;

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct FloatRepr<'t>(&'t str);

impl<'t> From<&'t str> for FloatRepr<'t> {
    fn from(value: &'t str) -> Self {
        Self(value)
    }
}

impl<'t> std::fmt::Display for FloatRepr<'t> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<'t> FloatRepr<'t> {
    pub fn to_float(&self) -> FloatType {
        self.0.parse().unwrap()
    }
}
