// Thanks to https://michael-f-bryan.github.io/static-analyser-in-rust/book/lex.html

use std::{cmp::Ordering, fmt::Write};

use crate::{
    error::{ErrorContext, ParseError, ParseResult},
    token::{IndentationType, QuoteMode},
};
use crate::{
    token::{FloatType, IntType, Keyword, Operator, Punct, Token, Value},
    NewLine,
};
use tracing::debug;

struct TokenView<'a>(&'a [char]);

impl<'a> std::fmt::Display for TokenView<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const MAX_ELEMS: usize = 20;

        f.write_char('"')?;

        for elem in self.0.iter().take(MAX_ELEMS) {
            match elem {
                '\n' => f.write_str("\\n")?,
                '\r' => f.write_str("\\r")?,
                c => f.write_char(*c)?,
            }
        }

        if self.0.len() > MAX_ELEMS {
            f.write_char('…')?;
        }

        f.write_char('"')?;

        Ok(())
    }
}

pub struct TokenReader {
    cursor: usize,
    remaining_text: Vec<char>,
}

/// Token reader context.
#[derive(Debug)]
pub struct TokenReaderContext {
    /// Previous read token.
    pub previous_token: Option<Token>,
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
}

impl TokenReaderContext {
    /// Get a position tuple.
    pub fn position(&self) -> (usize, usize) {
        (self.lines_seen, self.cols_seen)
    }
}

impl Default for TokenReaderContext {
    fn default() -> Self {
        Self {
            previous_token: None,
            indentation_type: IndentationType::Space,
            indentation_size: 4,
            indentation_seen: false,
            current_indent: 0,
            cols_seen: 0,
            lines_seen: 0,
        }
    }
}

struct TokenReadMethods;

impl TokenReadMethods {
    fn build_error(ctx: &TokenReaderContext, e: ParseError) -> (ParseError, ErrorContext) {
        (
            e,
            ErrorContext {
                col: ctx.cols_seen + 1,
                line: ctx.lines_seen + 1,
            },
        )
    }

    #[tracing::instrument(skip(predicate), fields(data = %TokenView(data)), ret)]
    fn take_while<'a, F>(
        ctx: &TokenReaderContext,
        data: &'a [char],
        mut predicate: F,
    ) -> ParseResult<(&'a [char], usize)>
    where
        F: FnMut(char) -> bool,
    {
        let mut current_index = 0;
        for ch in data {
            let should_continue = predicate(*ch);

            if !should_continue {
                break;
            }

            current_index += 1;
        }

        if current_index == 0 {
            Err(Self::build_error(ctx, ParseError::PredicateDoesNotMatch))
        } else {
            Ok((&data[..current_index], current_index))
        }
    }

    #[tracing::instrument(fields(data = %TokenView(data)), ret)]
    fn read_comment(ctx: &TokenReaderContext, data: &[char]) -> ParseResult<(Token, usize)> {
        let (comment, chars_read) =
            Self::take_while(ctx, &data[1..], |c| c != '\n' && c != '\r').unwrap_or((&[], 0));
        Ok((Token::Comment(comment.iter().collect()), chars_read + 1))
    }

    #[tracing::instrument(fields(data = %TokenView(data)), ret)]
    fn read_number(ctx: &TokenReaderContext, data: &[char]) -> ParseResult<(Token, usize)> {
        let mut seen_dot = false;

        let (decimal, chars_read) = Self::take_while(ctx, data, |c| {
            if c.is_ascii_digit() {
                true
            } else if c == '.' {
                if !seen_dot {
                    seen_dot = true;
                    true
                } else {
                    false
                }
            } else {
                false
            }
        })?;

        let decimal: String = decimal.iter().collect();
        if seen_dot {
            // Make sure the float is valid, before storing it in the token
            let _: FloatType = decimal
                .parse()
                .map_err(|e: std::num::ParseFloatError| Self::build_error(ctx, e.into()))?;
            Ok((Token::Value(Value::Float(decimal.into())), chars_read))
        } else {
            let n: IntType = decimal
                .parse()
                .map_err(|e: std::num::ParseIntError| Self::build_error(ctx, e.into()))?;
            Ok((Token::Value(Value::Int(n)), chars_read))
        }
    }

    #[tracing::instrument(fields(data = %TokenView(data)), ret)]
    fn read_string(ctx: &TokenReaderContext, data: &[char]) -> ParseResult<(Token, usize)> {
        let delimiter = data[0];
        let mut prev_char = delimiter;
        let mut count = 0;

        let (value, chars_read) = if data[1] != delimiter {
            Self::take_while(ctx, &data[1..], |c| {
                if prev_char == '\\' && c == delimiter {
                    count += 1;
                    prev_char = c;
                    true
                } else {
                    count += 1;
                    prev_char = c;
                    c != delimiter
                }
            })?
        } else {
            let value: &'static [char] = &[];
            (value, 0usize)
        };

        let quote_mode = if delimiter == '\'' {
            QuoteMode::Single
        } else {
            QuoteMode::Double
        };

        Ok((
            Token::Value(Value::String(value.iter().collect(), quote_mode)),
            chars_read + 2,
        ))
    }

    #[tracing::instrument(ret)]
    fn read_keyword(data: &str) -> Option<Keyword> {
        let tok = match data {
            "and" => Keyword::And,
            "assert" => Keyword::Assert,
            "break" => Keyword::Break,
            "breakpoint" => Keyword::Breakpoint,
            "case" => Keyword::Case,
            "class" => Keyword::Class,
            "class_name" => Keyword::ClassName,
            "const" => Keyword::Const,
            "continue" => Keyword::Continue,
            "elif" => Keyword::Elif,
            "else" => Keyword::Else,
            "enum" => Keyword::Enum,
            "export" => Keyword::Export,
            "extends" => Keyword::Extends,
            "for" => Keyword::For,
            "func" => Keyword::Func,
            "get" => Keyword::Get,
            "if" => Keyword::If,
            "in" => Keyword::In,
            "is" => Keyword::Is,
            "master" => Keyword::Master,
            "mastersync" => Keyword::MasterSync,
            "not" => Keyword::Not,
            "null" => Keyword::Null,
            "onready" => Keyword::OnReady,
            "or" => Keyword::Or,
            "pass" => Keyword::Pass,
            "puppet" => Keyword::Puppet,
            "puppetsync" => Keyword::PuppetSync,
            "remote" => Keyword::Remote,
            "remotesync" => Keyword::RemoteSync,
            "return" => Keyword::Return,
            "self" => Keyword::SelfKw,
            "set" => Keyword::Set,
            "setget" => Keyword::SetGet,
            "signal" => Keyword::Signal,
            "static" => Keyword::Static,
            "switch" => Keyword::Switch,
            "tool" => Keyword::Tool,
            "var" => Keyword::Var,
            "while" => Keyword::While,
            "yield" => Keyword::Yield,
            _ => return None,
        };

        Some(tok)
    }

    #[tracing::instrument(fields(data = %TokenView(data)), ret)]
    fn read_identifier(ctx: &TokenReaderContext, data: &[char]) -> ParseResult<(Token, usize)> {
        // identifiers can't start with a number
        match data.first() {
            Some(ch) if ch.is_ascii_digit() => {
                return Err(Self::build_error(ctx, ParseError::InvalidIdentifier))
            }
            None => return Err(Self::build_error(ctx, ParseError::UnexpectedEof)),
            _ => {}
        }

        let (got, chars_read) =
            Self::take_while(ctx, data, |ch| ch == '_' || ch.is_alphanumeric())?;
        let got: String = got.iter().collect();

        match &got[..] {
            "true" => return Ok((Token::Value(Value::Boolean(true)), 4)),
            "false" => return Ok((Token::Value(Value::Boolean(false)), 5)),
            _ => (),
        }

        match Self::read_keyword(&got) {
            Some(k) => Ok((Token::Keyword(k), chars_read)),
            None => Ok((Token::Identifier(got.to_string()), chars_read)),
        }
    }

    #[tracing::instrument(fields(data = %TokenView(data)), ret)]
    fn read_whitespace(ctx: &TokenReaderContext, data: &[char]) -> ParseResult<(Token, usize)> {
        let (s, chars_read) = Self::take_while(ctx, data, |ch| ch == ' ' || ch == '\t')?;
        Ok((Token::Whitespace(s.iter().collect()), chars_read))
    }

    #[tracing::instrument(fields(data = %TokenView(data)), ret)]
    fn try_read_indent(
        ctx: &mut TokenReaderContext,
        data: &[char],
    ) -> ParseResult<Option<(Vec<Token>, usize)>> {
        let mut toks = vec![];
        let mut count = 0;

        let first_char = data.first();
        match first_char {
            Some(' ' | '\t') => {
                // Indents or dedents
                let (this_indent, chars_read) =
                    Self::take_while(ctx, data, |ch| ch == ' ' || ch == '\t')?;
                count += chars_read;

                if !ctx.indentation_seen {
                    // Use this indent as a basis
                    ctx.indentation_seen = true;
                    ctx.indentation_type = match this_indent[0] {
                        ' ' => IndentationType::Space,
                        '\t' => IndentationType::Tab,
                        _ => unreachable!(),
                    };
                    ctx.current_indent += 1;
                    ctx.indentation_size = chars_read;
                    toks.push(Token::Indent);
                } else {
                    let current_indent_size = ctx.current_indent * ctx.indentation_size;
                    match current_indent_size.cmp(&chars_read) {
                        Ordering::Greater => {
                            let mut counter = current_indent_size - chars_read;
                            while counter > 0 {
                                counter -= ctx.indentation_size;
                                toks.push(Token::Dedent);
                                ctx.current_indent -= 1;
                            }
                        }
                        Ordering::Less => {
                            let mut counter = chars_read - current_indent_size;
                            while counter > 0 {
                                counter -= ctx.indentation_size;
                                toks.push(Token::Indent);
                                ctx.current_indent += 1;
                            }
                        }
                        Ordering::Equal => (),
                    }
                }

                Ok(Some((toks, count)))
            }
            Some(_) | None => {
                // Check for prev indent
                if ctx.indentation_seen {
                    for _ in 0..ctx.current_indent {
                        toks.push(Token::Dedent);
                    }
                    ctx.current_indent = 0;
                }

                if toks.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some((toks, count)))
                }
            }
        }
    }

    #[tracing::instrument(fields(data = %TokenView(data)), ret)]
    fn read_nodepath(ctx: &TokenReaderContext, data: &[char]) -> ParseResult<(Token, usize)> {
        let next = match data[1..].first() {
            Some(&c) => c,
            None => return Err(Self::build_error(ctx, ParseError::UnexpectedEof)),
        };

        if next == '\'' || next == '"' {
            let (tok, chars_read) = Self::read_string(ctx, &data[1..])?;
            let (value, quote_mode) = match tok {
                Token::Value(Value::String(s, m)) => (s, m),
                _ => unreachable!(),
            };

            Ok((Token::NodePath(value, Some(quote_mode)), chars_read + 1))
        } else {
            let (value, chars_read) = Self::take_while(ctx, &data[1..], |c| {
                c.is_alphanumeric() || c == '_' || c == '/'
            })?;

            Ok((
                Token::NodePath(value.iter().collect(), None),
                chars_read + 1,
            ))
        }
    }

    #[tracing::instrument(fields(data = %TokenView(data)), ret)]
    fn read_compound_tokens(
        ctx: &TokenReaderContext,
        data: &[char],
    ) -> ParseResult<Option<(Token, usize)>> {
        let first = match data.first() {
            Some(c) => c,
            None => return Err(Self::build_error(ctx, ParseError::UnexpectedEof)),
        };

        let second = match data.get(1) {
            Some(c) => c,
            None => return Ok(None),
        };

        let tok = match (first, second) {
            ('+', '=') => Token::Operator(Operator::AddAssign),
            ('&', '=') => Token::Operator(Operator::AndAssign),
            ('&', '&') => Token::Operator(Operator::BinAnd),
            ('=', '=') => Token::Operator(Operator::BinEq),
            ('!', '=') => Token::Operator(Operator::BinNotEq),
            ('|', '|') => Token::Operator(Operator::BinOr),
            ('<', '<') => Token::Operator(Operator::BitLShift),
            ('>', '>') => Token::Operator(Operator::BitRShift),
            ('/', '=') => Token::Operator(Operator::DivAssign),
            ('>', '=') => Token::Operator(Operator::GreaterOrEq),
            (':', '=') => Token::Operator(Operator::Infer),
            ('<', '=') => Token::Operator(Operator::LowerOrEq),
            ('%', '=') => Token::Operator(Operator::ModAssign),
            ('*', '=') => Token::Operator(Operator::MulAssign),
            ('|', '=') => Token::Operator(Operator::OrAssign),
            ('-', '=') => Token::Operator(Operator::SubAssign),
            ('-', '>') => Token::Operator(Operator::TypeArrow),
            ('.', '.') => Token::Operator(Operator::TwoDots),
            ('^', '=') => Token::Operator(Operator::XorAssign),
            (_, _) => return Ok(None),
        };

        Ok(Some((tok, 2)))
    }

    #[tracing::instrument(fields(data = %TokenView(data)), ret)]
    fn read_newline(ctx: &TokenReaderContext, data: &[char]) -> ParseResult<(Token, usize)> {
        match (data.first(), data.get(1)) {
            (Some('\r'), Some('\n')) => Ok((Token::NewLine(NewLine::CrLf), 2)),
            (Some('\n'), _) => Ok((Token::NewLine(NewLine::Lf), 1)),
            (_, _) => Err(Self::build_error(ctx, ParseError::UnexpectedEof)),
        }
    }

    #[tracing::instrument(fields(data = %TokenView(data)), ret)]
    fn read_next_tokens(
        ctx: &mut TokenReaderContext,
        data: &[char],
    ) -> ParseResult<(Vec<Token>, usize)> {
        // Try compound tokens
        if let Some((tok, sz)) = Self::read_compound_tokens(ctx, data)? {
            return Ok((vec![tok], sz));
        }

        // Detect indent
        if let Some(Token::NewLine(_)) = ctx.previous_token {
            if let Some((toks, count)) = Self::try_read_indent(ctx, data)? {
                debug!(
                    message = "read_next_tokens=>try_read_indent",
                    read = ?count,
                    first_char = ?data.first(),
                    position = ?ctx.position()
                );
                return Ok((toks, count));
            }
        }

        let next = match data.first() {
            Some(c) => c,
            None => return Err(Self::build_error(ctx, ParseError::UnexpectedEof)),
        };

        let (tok, length) = match next {
            '&' => (Token::Punct(Punct::Ampersand), 1),
            '*' => (Token::Punct(Punct::Asterisk), 1),
            '\\' => (Token::Punct(Punct::Backslash), 1),
            '^' => (Token::Punct(Punct::Caret), 1),
            '}' => (Token::Punct(Punct::ClosedCurly), 1),
            ')' => (Token::Punct(Punct::ClosedParens), 1),
            ']' => (Token::Punct(Punct::ClosedSquare), 1),
            ':' => (Token::Punct(Punct::Colon), 1),
            ',' => (Token::Punct(Punct::Comma), 1),
            '.' => (Token::Punct(Punct::Dot), 1),
            '=' => (Token::Punct(Punct::Eq), 1),
            '>' => (Token::Punct(Punct::Greater), 1),
            '<' => (Token::Punct(Punct::Lower), 1),
            '-' => (Token::Punct(Punct::Minus), 1),
            '!' => (Token::Punct(Punct::Not), 1),
            '{' => (Token::Punct(Punct::OpenCurly), 1),
            '(' => (Token::Punct(Punct::OpenParens), 1),
            '[' => (Token::Punct(Punct::OpenSquare), 1),
            '%' => (Token::Punct(Punct::Percent), 1),
            '|' => (Token::Punct(Punct::Pipe), 1),
            '+' => (Token::Punct(Punct::Plus), 1),
            ';' => (Token::Punct(Punct::Semicolon), 1),
            '/' => (Token::Punct(Punct::Slash), 1),
            '~' => (Token::Punct(Punct::Tilde), 1),
            '\n' | '\r' => Self::read_newline(ctx, data)?,
            '$' => Self::read_nodepath(ctx, data)?,
            '#' => Self::read_comment(ctx, data)?,
            '\'' | '"' => Self::read_string(ctx, data)?,
            '0'..='9' => Self::read_number(ctx, data)?,
            ' ' | '\t' => Self::read_whitespace(ctx, data)?,
            &c if c.is_ascii_alphabetic() || c == '_' => Self::read_identifier(ctx, data)?,
            &other => return Err(Self::build_error(ctx, ParseError::UnknownCharacter(other))),
        };

        Ok((vec![tok], length))
    }
}

/// Token span: a token with its position.
#[derive(Clone, Debug)]
pub struct TokenSpan {
    pub token: Token,
    pub start: usize,
    pub end: usize,
}

impl TokenReader {
    pub fn new(text: &str) -> Self {
        Self {
            cursor: 0,
            remaining_text: text.chars().collect(),
        }
    }

    pub fn next_tokens(&mut self, ctx: &mut TokenReaderContext) -> ParseResult<Vec<TokenSpan>> {
        if self.remaining_text.is_empty() {
            let mut tokens = vec![];

            // Handle newline with remaining indents
            if let Some(Token::NewLine(_)) = ctx.previous_token {
                for _ in 0..ctx.current_indent {
                    tokens.push(Token::Dedent);
                }
            }

            tokens.push(Token::Eof);

            let start = self.cursor;
            let end = self.cursor;

            Ok(tokens
                .into_iter()
                .map(|token| TokenSpan { token, start, end })
                .collect())
        } else {
            let start = self.cursor;
            let tokens = self._next_tokens(ctx)?;
            let end = self.cursor;

            Ok(tokens
                .into_iter()
                .map(|token| TokenSpan { token, start, end })
                .collect())
        }
    }

    fn _next_tokens(&mut self, ctx: &mut TokenReaderContext) -> ParseResult<Vec<Token>> {
        let (toks, chars_read) = TokenReadMethods::read_next_tokens(ctx, &self.remaining_text)?;
        self._chomp(chars_read);

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

    fn _chomp(&mut self, num_bytes: usize) {
        self.remaining_text = self.remaining_text[num_bytes..].into();
        self.cursor += num_bytes;
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        error::ParseResult,
        read::{TokenReadMethods, TokenReaderContext},
        token::{IndentationType, NewLine, QuoteMode},
        Token, Value,
    };

    fn parser(
        method: impl Fn(&TokenReaderContext, &[char]) -> ParseResult<(Token, usize)>,
    ) -> impl Fn(&str) -> Token {
        move |s| {
            let mut ctx = TokenReaderContext::default();
            method(&mut ctx, &s.chars().collect::<Vec<_>>()).unwrap().0
        }
    }

    fn parser_indent(
        method: impl Fn(&mut TokenReaderContext, &[char]) -> ParseResult<Option<(Vec<Token>, usize)>>,
    ) -> impl Fn(&mut TokenReaderContext, &str) -> Option<Vec<Token>> {
        move |ctx, s| {
            method(ctx, &s.chars().collect::<Vec<_>>())
                .unwrap()
                .map(|(a, _)| a)
        }
    }

    #[test]
    fn newline() {
        let run = |s: &str| {
            let ctx = TokenReaderContext::default();
            let data = s.chars().collect::<Vec<_>>();
            TokenReadMethods::read_newline(&ctx, &data).map(|(t, _s)| t)
        };

        assert_eq!(run("\n").unwrap(), Token::NewLine(NewLine::Lf));
        assert_eq!(run("\r\n").unwrap(), Token::NewLine(NewLine::CrLf));
        assert!(run("e").is_err());
    }

    #[test]
    fn string() {
        let run = parser(TokenReadMethods::read_string);

        assert_eq!(
            run("'abcd'"),
            Token::Value(Value::String("abcd".into(), QuoteMode::Single))
        );
        assert_eq!(
            run("\"abcd\""),
            Token::Value(Value::String("abcd".into(), QuoteMode::Double))
        );
        assert_eq!(
            run("''"),
            Token::Value(Value::String("".into(), QuoteMode::Single))
        );
        assert_eq!(
            run("\"\""),
            Token::Value(Value::String("".into(), QuoteMode::Double))
        );
    }

    #[test]
    fn comment() {
        let run = parser(TokenReadMethods::read_comment);

        assert_eq!(run("# abcd"), Token::Comment(" abcd".into()));
        assert_eq!(run("#abcd"), Token::Comment("abcd".into()));
    }

    #[test]
    fn number() {
        let run = parser(TokenReadMethods::read_number);

        assert_eq!(run("1234"), Token::Value(Value::Int(1234)));
        assert_eq!(run("1234.0"), Token::Value(Value::Float("1234.0".into())));
        assert_eq!(run("1234.1"), Token::Value(Value::Float("1234.1".into())));
    }

    #[test]
    fn indent() {
        let mut ctx = TokenReaderContext::default();
        let run = parser_indent(TokenReadMethods::try_read_indent);

        assert!(!ctx.indentation_seen);

        assert_eq!(run(&mut ctx, "    "), Some(vec![Token::Indent]));
        assert!(ctx.indentation_seen);
        assert_eq!(ctx.current_indent, 1);
        assert_eq!(ctx.indentation_type, IndentationType::Space);
        assert_eq!(ctx.indentation_size, 4);

        assert_eq!(run(&mut ctx, "    "), Some(vec![]));
        assert_eq!(run(&mut ctx, "        "), Some(vec![Token::Indent]));
        assert_eq!(ctx.current_indent, 2);

        assert_eq!(run(&mut ctx, "        "), Some(vec![]));
        assert_eq!(ctx.current_indent, 2);

        assert_eq!(run(&mut ctx, "    "), Some(vec![Token::Dedent]));
        assert_eq!(ctx.current_indent, 1);

        assert_eq!(run(&mut ctx, "    "), Some(vec![]));
        assert_eq!(ctx.current_indent, 1);

        assert_eq!(run(&mut ctx, "        "), Some(vec![Token::Indent]));
        assert_eq!(ctx.current_indent, 2);

        assert_eq!(run(&mut ctx, ""), Some(vec![Token::Dedent, Token::Dedent]));
        assert_eq!(ctx.current_indent, 0);
    }

    #[test]
    fn indent_with_code() {
        let mut ctx = TokenReaderContext::default();
        let run = parser_indent(TokenReadMethods::try_read_indent);

        assert!(!ctx.indentation_seen);

        assert_eq!(run(&mut ctx, "    if"), Some(vec![Token::Indent]));
        assert!(ctx.indentation_seen);
        assert_eq!(ctx.current_indent, 1);
        assert_eq!(ctx.indentation_type, IndentationType::Space);
        assert_eq!(ctx.indentation_size, 4);

        assert_eq!(run(&mut ctx, "    if"), Some(vec![]));
        assert_eq!(run(&mut ctx, "        if"), Some(vec![Token::Indent]));
        assert_eq!(ctx.current_indent, 2);

        assert_eq!(run(&mut ctx, "        if"), Some(vec![]));
        assert_eq!(ctx.current_indent, 2);

        assert_eq!(run(&mut ctx, "    if"), Some(vec![Token::Dedent]));
        assert_eq!(ctx.current_indent, 1);

        assert_eq!(run(&mut ctx, "    if"), Some(vec![]));
        assert_eq!(ctx.current_indent, 1);

        assert_eq!(run(&mut ctx, "        if"), Some(vec![Token::Indent]));
        assert_eq!(ctx.current_indent, 2);

        assert_eq!(
            run(&mut ctx, "if"),
            Some(vec![Token::Dedent, Token::Dedent])
        );
        assert_eq!(ctx.current_indent, 0);
    }

    #[test]
    fn nodepath() {
        let run = parser(TokenReadMethods::read_nodepath);

        assert_eq!(run("$A/B/C"), Token::NodePath("A/B/C".into(), None));
        assert_eq!(
            run("$'A /B/C'"),
            Token::NodePath("A /B/C".into(), Some(QuoteMode::Single))
        );
        assert_eq!(
            run("$\"A /B/C\""),
            Token::NodePath("A /B/C".into(), Some(QuoteMode::Double))
        );
    }
}
