// Thanks to https://michael-f-bryan.github.io/static-analyser-in-rust/book/lex.html

use crate::token::{FloatType, IntType, Keyword, Operator, Punct, Token, Value};
use crate::{
    error::{ErrorContext, ParseError, ParseResult},
    token::QuoteMode,
};

pub struct TokenReader {
    cursor: usize,
    remaining_text: Vec<char>,
}

#[derive(Default)]
pub struct TokenReaderContext {
    pub previous_token: Option<Token>,
    pub indent_sample: Option<(IndentationType, usize)>,
    pub cols_seen: usize,
    pub lines_seen: usize,
}

struct TokenReadUtils;

impl TokenReadUtils {
    fn build_error(ctx: &TokenReaderContext, e: ParseError) -> (ParseError, ErrorContext) {
        (
            e,
            ErrorContext {
                col: ctx.cols_seen + 1,
                line: ctx.lines_seen + 1,
            },
        )
    }

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

    fn read_comment(ctx: &TokenReaderContext, data: &[char]) -> ParseResult<(Token, usize)> {
        let (comment, chars_read) =
            Self::take_while(ctx, &data[1..], |c| c != '\n' && c != '\r').unwrap_or((&[], 0));
        Ok((Token::Comment(comment.iter().collect()), chars_read + 1))
    }

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

    fn read_ident(ctx: &TokenReaderContext, data: &[char]) -> ParseResult<(Token, usize)> {
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
            None => Ok((Token::Ident(got.to_string()), chars_read)),
        }
    }

    fn read_whitespace(ctx: &TokenReaderContext, data: &[char]) -> ParseResult<(Token, usize)> {
        let (s, chars_read) = Self::take_while(ctx, data, |ch| ch == ' ' || ch == '\t')?;
        Ok((Token::Whitespace(s.iter().collect()), chars_read))
    }

    fn read_indent(ctx: &mut TokenReaderContext, data: &[char]) -> ParseResult<(Token, usize)> {
        if let Some((sample_type, sample_size)) = &ctx.indent_sample {
            let char_to_scan = sample_type.as_char();

            let (_, chars_read) = Self::take_while(ctx, data, |ch| ch == char_to_scan)?;
            if chars_read % sample_size != 0 {
                return Err(Self::build_error(ctx, ParseError::InvalidIndentation));
            }

            Ok((Token::Indent(chars_read / sample_size), chars_read))
        } else {
            let first_char = data[0];
            let (_, chars_read) = Self::take_while(ctx, data, |ch| ch == first_char)?;
            let indentation_type = if first_char == ' ' {
                IndentationType::Space
            } else {
                IndentationType::Tab
            };

            ctx.indent_sample = Some((indentation_type, chars_read));
            Ok((Token::Indent(1), chars_read))
        }
    }

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

    fn read_two_tokens(
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

    fn read_single_token(
        ctx: &mut TokenReaderContext,
        data: &[char],
    ) -> ParseResult<(Token, usize)> {
        // Try two tokens
        if let Some(tok) = Self::read_two_tokens(ctx, data)? {
            return Ok(tok);
        }

        let next = match data.first() {
            Some(c) => c,
            None => return Err(Self::build_error(ctx, ParseError::UnexpectedEof)),
        };

        // Try using previous token
        #[allow(clippy::single_match)]
        match (&ctx.previous_token, next) {
            (Some(Token::LineFeed), ' ' | '\t') => return Self::read_indent(ctx, data),
            _ => (),
        }

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
            '\n' => (Token::LineFeed, 1),
            '\r' => (Token::CarriageReturn, 1),
            '$' => Self::read_nodepath(ctx, data)?,
            '#' => Self::read_comment(ctx, data)?,
            '\'' | '"' => Self::read_string(ctx, data)?,
            '0'..='9' => Self::read_number(ctx, data)?,
            ' ' | '\t' => Self::read_whitespace(ctx, data)?,
            &c if c.is_ascii_alphabetic() || c == '_' => Self::read_ident(ctx, data)?,
            &other => return Err(Self::build_error(ctx, ParseError::UnknownCharacter(other))),
        };

        Ok((tok, length))
    }
}

#[derive(Copy, Clone, Debug)]
pub enum IndentationType {
    Tab,
    Space,
}

impl IndentationType {
    pub fn as_char(&self) -> char {
        match self {
            Self::Tab => '\t',
            Self::Space => ' ',
        }
    }
}

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

    pub fn next_token(&mut self, ctx: &mut TokenReaderContext) -> ParseResult<TokenSpan> {
        if self.remaining_text.is_empty() {
            Ok(TokenSpan {
                token: Token::Eof,
                start: self.cursor,
                end: self.cursor,
            })
        } else {
            let start = self.cursor;
            let token = self._next_token(ctx)?;
            let end = self.cursor;

            Ok(TokenSpan { token, start, end })
        }
    }

    fn _next_token(&mut self, ctx: &mut TokenReaderContext) -> ParseResult<Token> {
        let (tok, chars_read) = TokenReadUtils::read_single_token(ctx, &self.remaining_text)?;
        self._chomp(chars_read);

        ctx.previous_token = Some(tok.clone());

        // Count lines and columns
        if tok == Token::LineFeed {
            ctx.lines_seen += 1;
            ctx.cols_seen = 0;
        } else {
            ctx.cols_seen += chars_read;
        }

        Ok(tok)
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
        read::{TokenReadUtils, TokenReaderContext},
        token::QuoteMode,
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

    fn parser_ctx(
        method: impl Fn(&mut TokenReaderContext, &[char]) -> ParseResult<(Token, usize)>,
    ) -> impl Fn(&mut TokenReaderContext, &str) -> Token {
        move |ctx, s| method(ctx, &s.chars().collect::<Vec<_>>()).unwrap().0
    }

    #[test]
    fn test_read_string() {
        let run = parser(TokenReadUtils::read_string);

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
    fn test_read_comment() {
        let run = parser(TokenReadUtils::read_comment);

        assert_eq!(run("# abcd"), Token::Comment(" abcd".into()));
        assert_eq!(run("#abcd"), Token::Comment("abcd".into()));
    }

    #[test]
    fn test_read_number() {
        let run = parser(TokenReadUtils::read_number);

        assert_eq!(run("1234"), Token::Value(Value::Int(1234)));
        assert_eq!(run("1234.0"), Token::Value(Value::Float("1234.0".into())));
        assert_eq!(run("1234.1"), Token::Value(Value::Float("1234.1".into())));
    }

    #[test]
    fn test_read_indent_4spaces() {
        let mut ctx = TokenReaderContext::default();
        let run = parser_ctx(TokenReadUtils::read_indent);

        assert_eq!(run(&mut ctx, "    "), Token::Indent(1));
        assert_eq!(run(&mut ctx, "        "), Token::Indent(2));
        assert_eq!(run(&mut ctx, "            "), Token::Indent(3));
        assert_eq!(run(&mut ctx, "    "), Token::Indent(1));
    }

    #[test]
    fn test_read_indent_2spaces() {
        let mut ctx = TokenReaderContext::default();
        let run = parser_ctx(TokenReadUtils::read_indent);

        assert_eq!(run(&mut ctx, "  "), Token::Indent(1));
        assert_eq!(run(&mut ctx, "    "), Token::Indent(2));
        assert_eq!(run(&mut ctx, "      "), Token::Indent(3));
        assert_eq!(run(&mut ctx, "  "), Token::Indent(1));
    }

    #[test]
    fn test_read_indent_1tab() {
        let mut ctx = TokenReaderContext::default();
        let run = parser_ctx(TokenReadUtils::read_indent);

        assert_eq!(run(&mut ctx, "\t"), Token::Indent(1));
        assert_eq!(run(&mut ctx, "\t\t"), Token::Indent(2));
        assert_eq!(run(&mut ctx, "\t\t\t"), Token::Indent(3));
        assert_eq!(run(&mut ctx, "\t"), Token::Indent(1));
    }

    #[test]
    fn test_read_indent_2tabs() {
        let mut ctx = TokenReaderContext::default();
        let run = parser_ctx(TokenReadUtils::read_indent);

        assert_eq!(run(&mut ctx, "\t\t"), Token::Indent(1));
        assert_eq!(run(&mut ctx, "\t\t\t\t"), Token::Indent(2));
        assert_eq!(run(&mut ctx, "\t\t\t\t\t\t"), Token::Indent(3));
        assert_eq!(run(&mut ctx, "\t\t"), Token::Indent(1));
    }

    #[test]
    fn test_read_nodepath() {
        let run = parser(TokenReadUtils::read_nodepath);

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
