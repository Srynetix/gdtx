// From https://michael-f-bryan.github.io/static-analyser-in-rust/book/lex.html

pub type IntType = i64;
pub type FloatType = f64;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Predicate does not match")]
    PredicateDoesNotMatch,

    #[error("Unexpected EOF")]
    UnexpectedEof,

    #[error("Wrong identifier")]
    WrongIdentifier,

    #[error("Unknown character: {0}")]
    UnknownCharacter(char),

    #[error(transparent)]
    WrongFloatValue(#[from] std::num::ParseFloatError),

    #[error(transparent)]
    WrongIntValue(#[from] std::num::ParseIntError)
}

type Result<T> = core::result::Result<T, Error>;

fn take_while<F>(data: &str, mut predicate: F) -> Result<(&str, usize)>
    where F: FnMut(char) -> bool
{
    let mut current_index = 0;
    for ch in data.chars() {
        let should_continue = predicate(ch);

        if !should_continue {
            break;
        }

        current_index += ch.len_utf8();
    }

    if current_index == 0 {
        Err(Error::PredicateDoesNotMatch)
    } else {
        Ok((&data[..current_index], current_index))
    }
}

fn read_number(data: &str) -> Result<(Token, usize)> {
    let mut seen_dot = false;

    let (decimal, bytes_read) = take_while(data, |c| {
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

    if seen_dot {
        let n: FloatType = decimal.parse()?;
        Ok((Token::Value(Value::Float(n)), bytes_read))
    } else {
        let n: IntType = decimal.parse()?;
        Ok((Token::Value(Value::Int(n)), bytes_read))
    }
}

fn read_ident(data: &str) -> Result<(Token, usize)> {
    // identifiers can't start with a number
    match data.chars().next() {
        Some(ch) if ch.is_ascii_digit() => return Err(Error::WrongIdentifier),
        None => return Err(Error::UnexpectedEof),
        _ => {},
    }

    let (got, bytes_read) = take_while(data, |ch| ch == '_' || ch.is_alphanumeric())?;

    // TODO: Recognise keywords using a `match` statement here.

    let tok = Token::Ident(got.to_string());
    Ok((tok, bytes_read))
}

fn read_whitespace(data: &str) -> Result<(Token, usize)> {
    let (s, bytes_read) = take_while(data, |ch| ch == ' ' || ch == '\t')?;
    Ok((Token::Whitespace(s.into()), bytes_read))
}

fn read_single_token(data: &str) -> Result<(Token, usize)> {
    let next = match data.chars().next() {
        Some(c) => c,
        None => return Err(Error::UnexpectedEof),
    };

    let (tok, length) = match next {
        '&' => (Token::Punct(Punct::Ampersand), 1),
        '*' => (Token::Punct(Punct::Asterisk), 1),
        '^' => (Token::Punct(Punct::Caret), 1),
        '}' => (Token::Punct(Punct::ClosedCurly), 1),
        ')' => (Token::Punct(Punct::ClosedParens), 1),
        ']' => (Token::Punct(Punct::ClosedSquare), 1),
        ':' => (Token::Punct(Punct::Colon), 1),
        '.' => (Token::Punct(Punct::Dot), 1),
        '=' => (Token::Punct(Punct::Eq), 1),
        '>' => (Token::Punct(Punct::Greater), 1),
        '#' => (Token::Punct(Punct::Hash), 1),
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
        '\n' => (Token::LineFeed, 1),
        '\r' => (Token::CarriageReturn, 1),
        '0' ..= '9' => read_number(data)?,
        ' ' | '\t' => read_whitespace(data)?,
        c @ '_' | c if c.is_alphabetic() => read_ident(data)?,
        other => return Err(Error::UnknownCharacter(other))
    };

    Ok((tok, length))
}

pub struct Lexer;

struct LexerInner {
    cursor: usize,
    remaining_text: String
}

impl LexerInner {
    pub fn new(text: &str) -> Self {
        Self {
            cursor: 0,
            remaining_text: text.into()
        }
    }

    fn next_token(&mut self) -> Result<(Token, usize, usize)> {
        if self.remaining_text.is_empty() {
            Ok((Token::Eof, self.cursor, self.cursor))
        } else {
            let start = self.cursor;
            let tok = self._next_token()?;
            let end = self.cursor;
            Ok((tok, start, end))
        }
    }

    fn _next_token(&mut self) -> Result<Token> {
        let (tok, bytes_read) = read_single_token(&self.remaining_text)?;
        self.chomp(bytes_read);

        Ok(tok)
    }

    fn chomp(&mut self, num_bytes: usize) {
        self.remaining_text = self.remaining_text[num_bytes..].into();
        self.cursor += num_bytes;
    }
}

impl Lexer {
    pub fn new() -> Self {
        Self
    }

    pub fn lex(&self, text: &str) -> Result<Vec<(Token, usize, usize)>> {
        let mut inner = LexerInner::new(text);
        let mut tokens = vec![];

        loop {
            let (tok, start, end) = inner.next_token()?;
            if tok == Token::Eof {
                tokens.push((tok, start, end));
                break;
            }

            tokens.push((tok, start, end));
        }

        Ok(tokens)
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Ident(String),
    Value(Value),
    Punct(Punct),
    Whitespace(String),
    CarriageReturn,
    LineFeed,
    Eof
}

#[derive(Debug, PartialEq)]
pub enum Punct {
    Ampersand,
    Asterisk,
    Caret,
    ClosedCurly,
    ClosedParens,
    ClosedSquare,
    Colon,
    Dot,
    Eq,
    Greater,
    Hash,
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
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Int(IntType),
    Float(FloatType),
    SingleQuotedString(String),
    DoubleQuotedString(String),
    Boolean(bool)
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Var,
    Export,
    Static,
    If,
    Elif,
    Else,
    While,
    For,
    OnReady,
    Switch,
    Case,
    Return,
    Break,
    Continue,
    Yield,
    Signal,
    Func,
    Class,
    Enum,
    Const
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_one() {
        let lexer = Lexer::new();
        assert_eq!(
            lexer.lex("1 + 1 + (a * 2)"),
            vec![
                Token::Value(Value::Int(1)),
                Token::Whitespace(" ".into()),
                Token::Punct(Punct::Plus),
                Token::Whitespace(" ".into()),
                Token::Value(Value::Int(1)),
                Token::Whitespace(" ".into()),
                Token::Operator(Operator::Plus),
                Token::Whitespace(" ".into()),
                Token::Punct(Punct::OpenParens),
                Token::Ident("a".into()),
                Token::Whitespace(" ".into()),
                Token::Operator(Operator::Multiply),
                Token::Whitespace(" ".into()),
                Token::Value(Value::Int(2)),
                Token::Punct(Punct::ClosedParens),
                Token::Eof
            ]
        )
    }
}