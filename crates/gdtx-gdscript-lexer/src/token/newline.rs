use serde::{Deserialize, Serialize};

/// Newline.
#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum NewLine {
    /// CRLF: "\r\n"
    CrLf,
    /// LF: "\n"
    Lf,
}
