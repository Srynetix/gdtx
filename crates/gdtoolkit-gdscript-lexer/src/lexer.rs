use crate::read::TokenReader;
use crate::token::Token;
use crate::{
    error::Result,
    read::{TokenReaderContext, TokenSpan},
};

/// GDScript lexer.
#[derive(Default)]
pub struct GdScriptLexer;

/// GDScript lexer output.
#[derive(Debug)]
pub struct GdScriptLexerOutput<'t> {
    ctx: TokenReaderContext<'t>,
    spans: Vec<TokenSpan<'t>>,
}

impl<'t> GdScriptLexerOutput<'t> {
    /// Get token reader context.
    pub fn context(&self) -> &TokenReaderContext<'t> {
        &self.ctx
    }

    /// Get tokens from spans.
    pub fn tokens(&self) -> Vec<Token<'t>> {
        self.spans.iter().map(|s| s.token.clone()).collect()
    }

    /// Get parsable tokens from spans.
    ///
    /// It will ignore whitespaces (but not newlines) and comments.
    pub fn parsable_tokens(&self) -> Vec<Token<'t>> {
        self.spans
            .iter()
            .filter(|&s| !matches!(s.token, Token::Whitespace(_) | Token::Comment(_)))
            .map(|s| &s.token)
            .cloned()
            .collect()
    }
}

impl GdScriptLexer {
    /// Read input code and generate tokens.
    pub fn lex<'a>(&self, text: &'a str) -> Result<GdScriptLexerOutput<'a>> {
        let reader = TokenReader::default();
        let mut ctx = TokenReaderContext::default();
        let mut tokens = vec![];

        loop {
            let mut spans = reader.next_tokens(&mut ctx, text)?;

            if !spans.is_empty() {
                let last_span = spans.last().unwrap();

                if last_span.token == Token::Eof {
                    tokens.append(&mut spans);
                    break;
                }

                tokens.append(&mut spans);
            }
        }

        Ok(GdScriptLexerOutput { ctx, spans: tokens })
    }
}
