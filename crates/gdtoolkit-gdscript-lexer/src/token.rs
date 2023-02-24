pub type IntType = i64;
pub type FloatType = f64;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    CarriageReturn,
    Comment(String),
    Eof,
    Ident(String),
    Indent(usize),
    Keyword(Keyword),
    LineFeed,
    NodePath(String, Option<QuoteMode>),
    Operator(Operator),
    Punct(Punct),
    Value(Value),
    Whitespace(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum QuoteMode {
    Single,
    Double,
}

#[derive(Debug, PartialEq, Copy, Clone)]
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

#[derive(Debug, PartialEq, Copy, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Boolean(bool),
    String(String, QuoteMode),
    Float(FloatRepr),
    Int(IntType),
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Copy, Clone)]
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
