use super::token::Token;

/// Token span: a token with its position.
#[derive(Clone, Debug)]
pub struct TokenSpan<'t> {
    pub token: Token<'t>,
    pub start: usize,
    pub end: usize,
}
