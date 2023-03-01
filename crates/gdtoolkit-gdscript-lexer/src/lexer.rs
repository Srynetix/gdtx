use crate::read::TokenReader;
use crate::token::Token;
use crate::{
    error::Result,
    read::{TokenReaderContext, TokenSpan},
};

#[derive(Default)]
pub struct GdScriptLexer;

#[derive(Debug)]
pub struct GdScriptLexerOutput {
    ctx: TokenReaderContext,
    spans: Vec<TokenSpan>,
}

impl GdScriptLexerOutput {
    pub fn context(&self) -> &TokenReaderContext {
        &self.ctx
    }

    pub fn tokens(&self) -> Vec<Token> {
        self.spans.iter().map(|s| s.token.clone()).collect()
    }

    pub fn parsable_tokens(&self) -> Vec<Token> {
        self.spans
            .iter()
            .filter(|&s| !matches!(s.token, Token::Whitespace(_) | Token::Comment(_)))
            .map(|s| &s.token)
            .cloned()
            .collect()
    }
}

impl GdScriptLexer {
    pub fn lex(&self, text: &str) -> Result<GdScriptLexerOutput> {
        let mut reader = TokenReader::new(text);
        let mut ctx = TokenReaderContext::default();
        let mut tokens = vec![];

        let text_length = text.len();
        let mut iterations = 0;

        loop {
            let mut spans = reader.next_tokens(&mut ctx)?;
            iterations += 1;

            if !spans.is_empty() {
                let last_span = spans.last().unwrap();

                if iterations % 10000 == 0 {
                    let amount_done = (last_span.start as f32 / text_length as f32) * 100.0;
                    println!("...{amount_done:0.2}%")
                }

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
