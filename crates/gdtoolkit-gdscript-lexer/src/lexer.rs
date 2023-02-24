use crate::read::TokenReader;
use crate::token::Token;
use crate::{
    error::Result,
    read::{TokenReaderContext, TokenSpan},
};

#[derive(Default)]
pub struct GdScriptLexer;

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
}

impl GdScriptLexer {
    pub fn lex(&self, text: &str) -> Result<GdScriptLexerOutput> {
        let mut reader = TokenReader::new(text);
        let mut ctx = TokenReaderContext::default();
        let mut tokens = vec![];

        let text_length = text.len();
        let mut iterations = 0;

        loop {
            let span = reader.next_token(&mut ctx)?;
            iterations += 1;

            if iterations % 10000 == 0 {
                let amount_done = (span.start as f32 / text_length as f32) * 100.0;
                println!("...{amount_done:0.2}%")
            }

            if span.token == Token::Eof {
                tokens.push(span);
                break;
            }

            tokens.push(span);
        }

        Ok(GdScriptLexerOutput { ctx, spans: tokens })
    }
}
