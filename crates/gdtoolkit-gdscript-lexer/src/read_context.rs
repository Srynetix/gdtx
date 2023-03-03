use super::token::{IndentationType, Token};

/// Token reader context.
#[derive(Debug)]
pub struct TokenReaderContext<'t> {
    /// Previous read token.
    pub previous_token: Option<Token<'t>>,
    /// Indentation type of the source code.
    pub indentation_type: IndentationType,
    /// Indentation size of the source code.
    pub indentation_size: usize,
    /// Mark if the indentation has already been detected in the source code.
    pub indentation_seen: bool,
    /// Current indent size.
    pub current_indent: usize,
    /// Columns seen in the source code.
    pub cols_seen: usize,
    /// Lines seen in the source code.
    pub lines_seen: usize,
    /// Cursor.
    pub cursor: usize,
}

impl<'t> TokenReaderContext<'t> {
    /// Get a position tuple.
    pub fn position(&self) -> (usize, usize) {
        (self.lines_seen, self.cols_seen)
    }
}

impl<'t> Default for TokenReaderContext<'t> {
    fn default() -> Self {
        Self {
            previous_token: None,
            indentation_type: IndentationType::Space,
            indentation_size: 4,
            indentation_seen: false,
            current_indent: 0,
            cols_seen: 0,
            lines_seen: 0,
            cursor: 0,
        }
    }
}
