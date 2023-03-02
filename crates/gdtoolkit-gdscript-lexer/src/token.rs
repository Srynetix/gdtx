use serde::{Deserialize, Serialize};

/// Int type alias.
pub type IntType = i64;
/// Float type alias.
pub type FloatType = f64;

/// Compact view, used for debug purposes.
pub struct CompactTokenView<'a>(pub &'a Token);

impl<'a> std::fmt::Display for CompactTokenView<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            Token::Comment(c) => f.write_fmt(format_args!("# {}", c)),
            Token::Dedent => f.write_str("<DDT>"),
            Token::Eof => f.write_str("<EOF>"),
            Token::Identifier(i) => f.write_fmt(format_args!("Id({:?})", i)),
            Token::Indent => f.write_str("<IDT>"),
            Token::Keyword(kw) => f.write_fmt(format_args!("Kw({:?})", kw)),
            Token::NewLine(t) => match t {
                NewLine::CrLf => f.write_str("<CRLF>"),
                NewLine::Lf => f.write_str("<LF>"),
            },
            Token::NodePath(p, q) => f.write_fmt(format_args!("P({}, {:?})", p, q)),
            Token::Operator(op) => f.write_fmt(format_args!("Op({:?})", op)),
            Token::Punct(p) => f.write_fmt(format_args!("Pt({:?})", p)),
            Token::Value(v) => f.write_fmt(format_args!("V({:?})", v)),
            Token::Whitespace(w) => f.write_fmt(format_args!("W({})", w)),
        }
    }
}

/// Token.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Token {
    Comment(String),
    Dedent,
    Eof,
    Identifier(String),
    Indent,
    Keyword(Keyword),
    NewLine(NewLine),
    NodePath(String, Option<QuoteMode>),
    Operator(Operator),
    Punct(Punct),
    Value(Value),
    Whitespace(String),
}

/// Newline.
#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum NewLine {
    /// CRLF: "\r\n"
    CrLf,
    /// LF: "\n"
    Lf,
}

/// Quote mode.
#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum QuoteMode {
    /// Single quotes: '
    Single,
    /// Double quotes: "
    Double,
}

/// Operator.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Copy, Clone, Serialize, Deserialize)]
pub enum Operator {
    AddAssign,
    AndAssign,
    BinAnd,
    BinEq,
    BinNotEq,
    BinOr,
    BitLShift,
    BitRShift,
    DivAssign,
    GreaterOrEq,
    Infer,
    LowerOrEq,
    ModAssign,
    MulAssign,
    OrAssign,
    SubAssign,
    TwoDots,
    TypeArrow,
    XorAssign,
}

/// Punctuation.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Copy, Clone, Serialize, Deserialize)]
pub enum Punct {
    Ampersand,
    Asterisk,
    Backslash,
    Caret,
    ClosedCurly,
    ClosedParens,
    ClosedSquare,
    Colon,
    Comma,
    Dot,
    Eq,
    Greater,
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
    Tilde,
}

/// Value.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Value {
    Boolean(bool),
    String(String, QuoteMode),
    Float(FloatRepr),
    Int(IntType),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct FloatRepr(String);

impl From<String> for FloatRepr {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl<'a> From<&'a str> for FloatRepr {
    fn from(value: &'a str) -> Self {
        Self(value.into())
    }
}

impl std::fmt::Display for FloatRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl FloatRepr {
    pub fn to_float(&self) -> FloatType {
        self.0.parse().unwrap()
    }
}

/// Indentation type.
#[derive(Copy, Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum IndentationType {
    /// Tab
    Tab,
    /// Space
    Space,
}

impl IndentationType {
    /// Get the type representation as a char.
    pub fn as_char(&self) -> char {
        match self {
            Self::Tab => '\t',
            Self::Space => ' ',
        }
    }
}

/// Keyword.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Copy, Clone, Serialize, Deserialize)]
pub enum Keyword {
    And,
    Assert,
    Break,
    Breakpoint,
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
    Get,
    If,
    In,
    Is,
    Master,
    MasterSync,
    Not,
    Null,
    OnReady,
    Or,
    Pass,
    Puppet,
    PuppetSync,
    Remote,
    RemoteSync,
    Return,
    SelfKw,
    Set,
    SetGet,
    Signal,
    Static,
    Switch,
    Tool,
    Var,
    While,
    Yield,
}
