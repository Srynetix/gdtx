#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Predicate does not match")]
    PredicateDoesNotMatch,

    #[error("Unexpected EOF")]
    UnexpectedEof,

    #[error("Invalid identifier")]
    InvalidIdentifier,

    #[error("Invalid indentation")]
    InvalidIndentation,

    #[error("Unknown character: {0}")]
    UnknownCharacter(char),

    #[error(transparent)]
    WrongFloatValue(#[from] std::num::ParseFloatError),

    #[error(transparent)]
    WrongIntValue(#[from] std::num::ParseIntError),
}

#[derive(Debug)]
pub struct ErrorContext {
    pub line: usize,
    pub col: usize,
}

/// Lexer error.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// Parsing error.
    #[error("Error while parsing: {source} (line {0}, col {1})", context.line, context.col)]
    ParseError {
        /// Source.
        source: ParseError,
        /// Context.
        context: ErrorContext,
    },
}

impl From<(ParseError, ErrorContext)> for Error {
    fn from((source, context): (ParseError, ErrorContext)) -> Self {
        Self::ParseError { source, context }
    }
}

pub type ParseResult<T> = core::result::Result<T, (ParseError, ErrorContext)>;

/// Lexer result alias.
pub type Result<T> = core::result::Result<T, Error>;
