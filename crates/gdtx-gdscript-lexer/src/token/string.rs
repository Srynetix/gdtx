use serde::{Deserialize, Serialize};

/// Quote mode.
#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum QuoteMode {
    /// Single quotes: '
    Single,
    /// Double quotes: "
    Double,
}
