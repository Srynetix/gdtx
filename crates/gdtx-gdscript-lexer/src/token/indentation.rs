use serde::{Deserialize, Serialize};

/// Indentation type.
#[derive(Copy, Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum IndentationType {
    /// Tab
    Tab,
    /// Space
    Space,
}

impl IndentationType {
    /// Get the type representation as a char.
    pub fn as_char(&self) -> char {
        match self {
            Self::Tab => '\t',
            Self::Space => ' ',
        }
    }
}
