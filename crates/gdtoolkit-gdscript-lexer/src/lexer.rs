use crate::{
    error::ParseResult, readers::read_next_tokens, span::TokenSpan, Result, Token,
    TokenReaderContext,
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
        let mut ctx = TokenReaderContext::default();
        let mut tokens = vec![];

        loop {
            let mut spans = self.next_token_spans(&mut ctx, text)?;

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

    fn next_token_spans<'t>(
        &self,
        ctx: &mut TokenReaderContext<'t>,
        remaining_text: &'t str,
    ) -> ParseResult<Vec<TokenSpan<'t>>> {
        if remaining_text[ctx.cursor..].is_empty() {
            let mut tokens = vec![];

            // Handle newline with remaining indents
            if let Some(Token::NewLine(_)) = ctx.previous_token {
                for _ in 0..ctx.current_indent {
                    tokens.push(Token::Dedent);
                }
            }

            tokens.push(Token::Eof);

            let start = ctx.cursor;
            let end = ctx.cursor;

            Ok(tokens
                .into_iter()
                .map(|token| TokenSpan { token, start, end })
                .collect())
        } else {
            let start = ctx.cursor;
            let tokens = self.next_tokens(ctx, &remaining_text[ctx.cursor..])?;
            let end = ctx.cursor;

            Ok(tokens
                .into_iter()
                .map(|token| TokenSpan { token, start, end })
                .collect())
        }
    }

    fn next_tokens<'t>(
        &self,
        ctx: &mut TokenReaderContext<'t>,
        text: &'t str,
    ) -> ParseResult<Vec<Token<'t>>> {
        let (toks, chars_read) = read_next_tokens(ctx, text)?;
        ctx.cursor += chars_read;

        if !toks.is_empty() {
            ctx.previous_token = toks.last().cloned();

            // Count lines and columns
            if toks.iter().any(|n| matches!(n, Token::NewLine(_))) {
                ctx.lines_seen += 1;
                ctx.cols_seen = 0;
            } else {
                ctx.cols_seen += chars_read;
            }
        } else {
            ctx.previous_token = None;
        }

        Ok(toks)
    }
}
