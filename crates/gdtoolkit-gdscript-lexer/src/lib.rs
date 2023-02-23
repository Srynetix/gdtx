// From https://michael-f-bryan.github.io/static-analyser-in-rust/book/lex.html

/*
 * TODO:
 *      Strings
 *      Comments
 *      Indentation
 *      Boolean
 */

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
    WrongIntValue(#[from] std::num::ParseIntError),
}

type Result<T> = core::result::Result<T, Error>;

fn take_while<F>(data: &str, mut predicate: F) -> Result<(&str, usize)>
where
    F: FnMut(char) -> bool,
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

fn read_keyword(data: &str) -> Option<Keyword> {
    let tok = match data {
        "break" => Keyword::Break,
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
        "if" => Keyword::If,
        "onready" => Keyword::OnReady,
        "pass" => Keyword::Pass,
        "return" => Keyword::Return,
        "signal" => Keyword::Signal,
        "static" => Keyword::Static,
        "switch" => Keyword::Switch,
        "var" => Keyword::Var,
        "while" => Keyword::While,
        "yield" => Keyword::Yield,
        _ => return None,
    };

    Some(tok)
}

fn read_ident(data: &str) -> Result<(Token, usize)> {
    // identifiers can't start with a number
    match data.chars().next() {
        Some(ch) if ch.is_ascii_digit() => return Err(Error::WrongIdentifier),
        None => return Err(Error::UnexpectedEof),
        _ => {}
    }

    let (got, bytes_read) = take_while(data, |ch| ch == '_' || ch.is_alphanumeric())?;

    match read_keyword(got) {
        Some(k) => Ok((Token::Keyword(k), bytes_read)),
        None => Ok((Token::Ident(got.to_string()), bytes_read)),
    }
}

fn read_whitespace(data: &str) -> Result<(Token, usize)> {
    let (s, bytes_read) = take_while(data, |ch| ch == ' ' || ch == '\t')?;
    Ok((Token::Whitespace(s.into()), bytes_read))
}

fn read_size2_token(data: &str) -> Result<Option<Token>> {
    let mut chars = data.chars();

    let first = match chars.next() {
        Some(c) => c,
        None => return Err(Error::UnexpectedEof),
    };

    let second = match chars.next() {
        Some(c) => c,
        None => return Ok(None),
    };

    let tok = match (first, second) {
        ('+', '=') => Token::Operator(Operator::AddAssign),
        ('&', '&') => Token::Operator(Operator::And),
        ('/', '=') => Token::Operator(Operator::DivAssign),
        ('=', '=') => Token::Operator(Operator::Eq),
        (':', '=') => Token::Operator(Operator::Infer),
        ('%', '=') => Token::Operator(Operator::ModAssign),
        ('!', '=') => Token::Operator(Operator::NotEq),
        ('|', '|') => Token::Operator(Operator::Or),
        ('-', '=') => Token::Operator(Operator::SubAssign),
        ('-', '>') => Token::Operator(Operator::TypeArrow),
        (_, _) => return Ok(None),
    };

    Ok(Some(tok))
}

fn read_single_token(data: &str) -> Result<(Token, usize)> {
    // First, try size2 token
    if let Some(tok) = read_size2_token(data)? {
        return Ok((tok, 2));
    }

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
        '0'..='9' => read_number(data)?,
        ' ' | '\t' => read_whitespace(data)?,
        c @ '_' | c if c.is_alphabetic() => read_ident(data)?,
        other => return Err(Error::UnknownCharacter(other)),
    };

    Ok((tok, length))
}

pub struct Lexer;

struct LexerInner {
    cursor: usize,
    remaining_text: String,
}

impl LexerInner {
    pub fn new(text: &str) -> Self {
        Self {
            cursor: 0,
            remaining_text: text.into(),
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
    Operator(Operator),
    Punct(Punct),
    Whitespace(String),
    CarriageReturn,
    LineFeed,
    Eof,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    AddAssign,
    And,
    DivAssign,
    Eq,
    Infer,
    ModAssign,
    MulAssign,
    NotEq,
    Or,
    SubAssign,
    TypeArrow,
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
    Boolean(bool),
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Break,
    Case,
    Class,
    ClassName,
    Const,
    Continue,
    Elif,
    Else,
    Enum,
    Export,
    Extends,
    For,
    Func,
    If,
    OnReady,
    Pass,
    Return,
    Signal,
    Static,
    Switch,
    Var,
    While,
    Yield,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tok_int(value: IntType) -> Token {
        Token::Value(Value::Int(value))
    }

    fn tok_kw(value: Keyword) -> Token {
        Token::Keyword(value)
    }

    fn tok_ws(value: &str) -> Token {
        Token::Whitespace(value.into())
    }

    fn tok_op(value: Operator) -> Token {
        Token::Operator(value)
    }

    fn tok_ident(value: &str) -> Token {
        Token::Ident(value.into())
    }

    fn tok_punct(value: Punct) -> Token {
        Token::Punct(value)
    }

    fn lex_tokens(text: &str) -> Vec<Token> {
        Lexer::new()
            .lex(text)
            .unwrap()
            .into_iter()
            .map(|(t, _start, _end)| t)
            .collect()
    }

    #[test]
    fn test_expr() {
        assert_eq!(
            lex_tokens("1 += 1abcd + (a * 2) && 5 & 1"),
            vec![
                tok_int(1),
                tok_ws(" "),
                tok_op(Operator::AddAssign),
                tok_ws(" "),
                tok_int(1),
                tok_ident("abcd"),
                tok_ws(" "),
                tok_punct(Punct::Plus),
                tok_ws(" "),
                tok_punct(Punct::OpenParens),
                tok_ident("a"),
                tok_ws(" "),
                tok_punct(Punct::Asterisk),
                tok_ws(" "),
                tok_int(2),
                tok_punct(Punct::ClosedParens),
                tok_ws(" "),
                tok_op(Operator::And),
                tok_ws(" "),
                tok_int(5),
                tok_ws(" "),
                tok_punct(Punct::Ampersand),
                tok_ws(" "),
                tok_int(1),
                Token::Eof
            ]
        )
    }

    #[test]
    fn test_func() {
        assert_eq!(
            lex_tokens("static func dummy(a: int) -> void:\n    pass"),
            vec![
                tok_kw(Keyword::Static),
                tok_ws(" "),
                tok_kw(Keyword::Func),
                tok_ws(" "),
                tok_ident("dummy"),
                tok_punct(Punct::OpenParens),
                tok_ident("a"),
                tok_punct(Punct::Colon),
                tok_ws(" "),
                tok_ident("int"),
                tok_punct(Punct::ClosedParens),
                tok_ws(" "),
                tok_op(Operator::TypeArrow),
                tok_ws(" "),
                tok_ident("void"),
                tok_punct(Punct::Colon),
                Token::LineFeed,
                tok_ws("    "),
                tok_kw(Keyword::Pass),
                Token::Eof
            ]
        )
    }
}
