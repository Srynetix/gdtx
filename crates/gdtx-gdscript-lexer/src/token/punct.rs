use serde::{Deserialize, Serialize};

/// Punctuation.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Copy, Clone, Serialize, Deserialize)]
pub enum Punct {
    Ampersand,
    Asterisk,
    Backslash,
    Caret,
    ClosedCurly,
    ClosedParens,
    ClosedSquare,
    Colon,
    Comma,
    Dot,
    Eq,
    Greater,
    Lower,
    Minus,
    Not,
    OpenCurly,
    OpenParens,
    OpenSquare,
    Percent,
    Pipe,
    Plus,
    Semicolon,
    Slash,
    Tilde,
}
