use std::{borrow::Cow, io::Write};

use gdtoolkit_gdscript_lexer::{
    Keyword, Operator, Punct, QuoteMode, Token, TokenReaderContext, Value,
};

#[derive(Default)]
pub struct GdScriptWriter;

#[derive(Default)]
pub struct TokenWriter;

impl GdScriptWriter {
    pub fn write_tokens<W: Write>(
        &self,
        ctx: &TokenReaderContext,
        writer: &mut W,
        tokens: Vec<Token>,
    ) -> std::io::Result<()> {
        let token_writer = TokenWriter::default();
        for token in tokens {
            write!(writer, "{}", token_writer.token_to_string(ctx, token))?;
        }

        Ok(())
    }
}

impl TokenWriter {
    pub fn token_to_string(&self, ctx: &TokenReaderContext, token: Token) -> Cow<str> {
        match token {
            Token::CarriageReturn => Cow::Borrowed("\r"),
            Token::Comment(comment) => Cow::Owned(format!("#{comment}")),
            Token::Eof => Cow::Borrowed(""),
            Token::Ident(ident) => Cow::Owned(ident),
            Token::Indent(count) => {
                let (indent_type, indent_size) = ctx.indent_sample.unwrap();
                let char_to_copy = indent_type.as_char();
                let repeated = [char_to_copy].repeat(indent_size * count);
                Cow::Owned(repeated.into_iter().collect())
            }
            Token::Keyword(k) => match k {
                Keyword::And => Cow::Borrowed("and"),
                Keyword::Assert => Cow::Borrowed("assert"),
                Keyword::Break => Cow::Borrowed("break"),
                Keyword::Breakpoint => Cow::Borrowed("breakpoint"),
                Keyword::Case => Cow::Borrowed("case"),
                Keyword::Class => Cow::Borrowed("class"),
                Keyword::ClassName => Cow::Borrowed("class_name"),
                Keyword::Const => Cow::Borrowed("const"),
                Keyword::Continue => Cow::Borrowed("continue"),
                Keyword::Elif => Cow::Borrowed("elif"),
                Keyword::Else => Cow::Borrowed("else"),
                Keyword::Enum => Cow::Borrowed("enum"),
                Keyword::Export => Cow::Borrowed("export"),
                Keyword::Extends => Cow::Borrowed("extends"),
                Keyword::For => Cow::Borrowed("for"),
                Keyword::Func => Cow::Borrowed("func"),
                Keyword::Get => Cow::Borrowed("get"),
                Keyword::If => Cow::Borrowed("if"),
                Keyword::In => Cow::Borrowed("in"),
                Keyword::Is => Cow::Borrowed("is"),
                Keyword::Master => Cow::Borrowed("master"),
                Keyword::MasterSync => Cow::Borrowed("mastersync"),
                Keyword::Not => Cow::Borrowed("not"),
                Keyword::Null => Cow::Borrowed("null"),
                Keyword::OnReady => Cow::Borrowed("onready"),
                Keyword::Or => Cow::Borrowed("or"),
                Keyword::Pass => Cow::Borrowed("pass"),
                Keyword::Puppet => Cow::Borrowed("puppet"),
                Keyword::PuppetSync => Cow::Borrowed("puppetsync"),
                Keyword::Remote => Cow::Borrowed("remote"),
                Keyword::RemoteSync => Cow::Borrowed("remotesync"),
                Keyword::Return => Cow::Borrowed("return"),
                Keyword::SelfKw => Cow::Borrowed("self"),
                Keyword::Set => Cow::Borrowed("set"),
                Keyword::SetGet => Cow::Borrowed("setget"),
                Keyword::Signal => Cow::Borrowed("signal"),
                Keyword::Static => Cow::Borrowed("static"),
                Keyword::Switch => Cow::Borrowed("switch"),
                Keyword::Tool => Cow::Borrowed("tool"),
                Keyword::Var => Cow::Borrowed("var"),
                Keyword::While => Cow::Borrowed("while"),
                Keyword::Yield => Cow::Borrowed("yield"),
            },
            Token::LineFeed => Cow::Borrowed("\n"),
            Token::NodePath(s, mode) => match mode {
                None => Cow::Owned(format!("${s}")),
                Some(QuoteMode::Single) => Cow::Owned(format!("$'{s}'")),
                Some(QuoteMode::Double) => Cow::Owned(format!("$\"{s}\"")),
            },
            Token::Operator(op) => match op {
                Operator::AddAssign => Cow::Borrowed("+="),
                Operator::AndAssign => Cow::Borrowed("&="),
                Operator::BinAnd => Cow::Borrowed("&&"),
                Operator::BinEq => Cow::Borrowed("=="),
                Operator::BinNotEq => Cow::Borrowed("!="),
                Operator::BinOr => Cow::Borrowed("||"),
                Operator::BitLShift => Cow::Borrowed("<<"),
                Operator::BitRShift => Cow::Borrowed(">>"),
                Operator::DivAssign => Cow::Borrowed("/="),
                Operator::GreaterOrEq => Cow::Borrowed(">="),
                Operator::Infer => Cow::Borrowed(":="),
                Operator::LowerOrEq => Cow::Borrowed("<="),
                Operator::ModAssign => Cow::Borrowed("%="),
                Operator::MulAssign => Cow::Borrowed("*="),
                Operator::OrAssign => Cow::Borrowed("|="),
                Operator::SubAssign => Cow::Borrowed("-="),
                Operator::TwoDots => Cow::Borrowed(".."),
                Operator::TypeArrow => Cow::Borrowed("->"),
                Operator::XorAssign => Cow::Borrowed("^="),
            },
            Token::Punct(punct) => match punct {
                Punct::Ampersand => Cow::Borrowed("&"),
                Punct::Asterisk => Cow::Borrowed("*"),
                Punct::Backslash => Cow::Borrowed("\\"),
                Punct::Caret => Cow::Borrowed("^"),
                Punct::ClosedCurly => Cow::Borrowed("}"),
                Punct::ClosedParens => Cow::Borrowed(")"),
                Punct::ClosedSquare => Cow::Borrowed("]"),
                Punct::Colon => Cow::Borrowed(":"),
                Punct::Comma => Cow::Borrowed(","),
                Punct::Dot => Cow::Borrowed("."),
                Punct::Eq => Cow::Borrowed("="),
                Punct::Greater => Cow::Borrowed(">"),
                Punct::Lower => Cow::Borrowed("<"),
                Punct::Minus => Cow::Borrowed("-"),
                Punct::Not => Cow::Borrowed("!"),
                Punct::OpenCurly => Cow::Borrowed("{"),
                Punct::OpenParens => Cow::Borrowed("("),
                Punct::OpenSquare => Cow::Borrowed("["),
                Punct::Percent => Cow::Borrowed("%"),
                Punct::Pipe => Cow::Borrowed("|"),
                Punct::Plus => Cow::Borrowed("+"),
                Punct::Semicolon => Cow::Borrowed(";"),
                Punct::Slash => Cow::Borrowed("/"),
                Punct::Tilde => Cow::Borrowed("~"),
            },
            Token::Value(value) => match value {
                Value::Boolean(b) => {
                    if b {
                        Cow::Borrowed("true")
                    } else {
                        Cow::Borrowed("false")
                    }
                }
                Value::String(s, mode) => match mode {
                    QuoteMode::Single => Cow::Owned(format!("'{s}'")),
                    QuoteMode::Double => Cow::Owned(format!("\"{s}\"")),
                },
                Value::Float(f) => Cow::Owned(format!("{f}")),
                Value::Int(i) => Cow::Owned(format!("{i}")),
            },
            Token::Whitespace(w) => Cow::Owned(w),
        }
    }
}

#[cfg(test)]
mod tests {
    use gdtoolkit_gdscript_lexer::GdScriptLexer;
    use indoc::indoc;

    use super::GdScriptWriter;

    fn assert_format_same(source: &str) {
        let writer = GdScriptWriter::default();
        let lexer = GdScriptLexer::default();
        let lexer_output = lexer.lex(source).unwrap();

        let mut write_output: Vec<u8> = Vec::new();
        writer
            .write_tokens(
                lexer_output.context(),
                &mut write_output,
                lexer_output.tokens(),
            )
            .unwrap();
        let write_output = String::from_utf8(write_output).unwrap();

        assert_eq!(source, write_output)
    }

    #[test]
    fn test_expr_1() {
        assert_format_same("a + 1 += 1234");
    }

    #[test]
    fn test_script() {
        assert_format_same(indoc! {r#"
            # Sample
            extends Object
            class_name Hello

            static func sample(a: int) -> void:
                print("Hello !")

            func _a():
                pass
        "#});
    }
}
