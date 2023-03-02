// Thanks to https://michael-f-bryan.github.io/static-analyser-in-rust/book/lex.html

use std::cmp::Ordering;

use crate::{
    debug::TokenView,
    error::{ErrorContext, ParseError, ParseResult},
    token::{IndentationType, QuoteMode},
    TokenReaderContext,
};
use crate::{
    token::{FloatType, IntType, Keyword, Operator, Punct, Token, Value},
    NewLine,
};
use tracing::debug;

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
fn take_while<'t, F>(
    ctx: &TokenReaderContext<'t>,
    data: &'t str,
    mut predicate: F,
) -> ParseResult<(&'t str, usize)>
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
        Err(build_error(ctx, ParseError::PredicateDoesNotMatch))
    } else {
        Ok((&data[..current_index], current_index))
    }
}

#[tracing::instrument(fields(data = %TokenView(data)), ret)]
fn read_comment<'t>(
    ctx: &TokenReaderContext<'t>,
    data: &'t str,
) -> ParseResult<(Token<'t>, usize)> {
    let (comment, chars_read) =
        take_while(ctx, &data[1..], |c| c != '\n' && c != '\r').unwrap_or(("", 0));
    Ok((Token::Comment(comment), chars_read + 1))
}

#[tracing::instrument(fields(data = %TokenView(data)), ret)]
fn read_number<'t>(ctx: &TokenReaderContext<'t>, data: &'t str) -> ParseResult<(Token<'t>, usize)> {
    let mut seen_dot = false;

    let (decimal, chars_read) = take_while(ctx, data, |c| {
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
        let _: FloatType = decimal
            .parse()
            .map_err(|e: std::num::ParseFloatError| build_error(ctx, e.into()))?;
        Ok((Token::Value(Value::Float(decimal.into())), chars_read))
    } else {
        let n: IntType = decimal
            .parse()
            .map_err(|e: std::num::ParseIntError| build_error(ctx, e.into()))?;
        Ok((Token::Value(Value::Int(n)), chars_read))
    }
}

#[tracing::instrument(fields(data = %TokenView(data)), ret)]
fn read_string<'t>(ctx: &TokenReaderContext<'t>, data: &'t str) -> ParseResult<(Token<'t>, usize)> {
    let mut chars = data.chars();
    let delimiter = chars.next().unwrap();
    let mut prev_char = delimiter;
    let mut count = 0;

    let (value, chars_read) = if chars.next().unwrap() != delimiter {
        take_while(ctx, &data[1..], |c| {
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
        ("", 0usize)
    };

    let quote_mode = if delimiter == '\'' {
        QuoteMode::Single
    } else {
        QuoteMode::Double
    };

    Ok((
        Token::Value(Value::String(value, quote_mode)),
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
fn read_identifier<'t>(
    ctx: &TokenReaderContext<'t>,
    data: &'t str,
) -> ParseResult<(Token<'t>, usize)> {
    // identifiers can't start with a number
    match data.chars().next() {
        Some(ch) if ch.is_ascii_digit() => {
            return Err(build_error(ctx, ParseError::InvalidIdentifier))
        }
        None => return Err(build_error(ctx, ParseError::UnexpectedEof)),
        _ => {}
    }

    let (got, chars_read) = take_while(ctx, data, |ch| ch == '_' || ch.is_alphanumeric())?;

    match got {
        "true" => return Ok((Token::Value(Value::Boolean(true)), 4)),
        "false" => return Ok((Token::Value(Value::Boolean(false)), 5)),
        _ => (),
    }

    match read_keyword(got) {
        Some(k) => Ok((Token::Keyword(k), chars_read)),
        None => Ok((Token::Identifier(got), chars_read)),
    }
}

#[tracing::instrument(fields(data = %TokenView(data)), ret)]
fn read_whitespace<'t>(
    ctx: &TokenReaderContext<'t>,
    data: &'t str,
) -> ParseResult<(Token<'t>, usize)> {
    let (s, chars_read) = take_while(ctx, data, |ch| ch == ' ' || ch == '\t')?;
    Ok((Token::Whitespace(s), chars_read))
}

#[tracing::instrument(fields(data = %TokenView(data)), ret)]
fn try_read_indent<'t>(
    ctx: &mut TokenReaderContext<'t>,
    data: &'t str,
) -> ParseResult<Option<(Vec<Token<'t>>, usize)>> {
    let mut toks = vec![];
    let mut count = 0;

    let first_char = data.chars().next();
    match first_char {
        Some(' ' | '\t') => {
            // Indents or dedents
            let (this_indent, chars_read) = take_while(ctx, data, |ch| ch == ' ' || ch == '\t')?;
            count += chars_read;

            if !ctx.indentation_seen {
                // Use this indent as a basis
                ctx.indentation_seen = true;
                ctx.indentation_type = match this_indent.chars().next().unwrap() {
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
fn read_nodepath<'t>(
    ctx: &TokenReaderContext<'t>,
    data: &'t str,
) -> ParseResult<(Token<'t>, usize)> {
    let next = match data[1..].chars().next() {
        Some(c) => c,
        None => return Err(build_error(ctx, ParseError::UnexpectedEof)),
    };

    if next == '\'' || next == '"' {
        let (tok, chars_read) = read_string(ctx, &data[1..])?;
        let (value, quote_mode) = match tok {
            Token::Value(Value::String(s, m)) => (s, m),
            _ => unreachable!(),
        };

        Ok((Token::NodePath(value, Some(quote_mode)), chars_read + 1))
    } else {
        let (value, chars_read) = take_while(ctx, &data[1..], |c| {
            c.is_alphanumeric() || c == '_' || c == '/'
        })?;

        Ok((Token::NodePath(value, None), chars_read + 1))
    }
}

#[tracing::instrument(fields(data = %TokenView(data)), ret)]
fn read_compound_tokens<'t>(
    ctx: &TokenReaderContext<'t>,
    data: &'t str,
) -> ParseResult<Option<(Token<'t>, usize)>> {
    let mut chars = data.chars();

    let first = match chars.next() {
        Some(c) => c,
        None => return Err(build_error(ctx, ParseError::UnexpectedEof)),
    };

    let second = match chars.next() {
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
fn read_newline<'t>(
    ctx: &TokenReaderContext<'t>,
    data: &'t str,
) -> ParseResult<(Token<'t>, usize)> {
    let mut chars = data.chars();

    match (chars.next(), chars.next()) {
        (Some('\r'), Some('\n')) => Ok((Token::NewLine(NewLine::CrLf), 2)),
        (Some('\n'), _) => Ok((Token::NewLine(NewLine::Lf), 1)),
        (_, _) => Err(build_error(ctx, ParseError::UnexpectedEof)),
    }
}

#[tracing::instrument(fields(data = %TokenView(data)), ret)]
pub(crate) fn read_next_tokens<'t>(
    ctx: &mut TokenReaderContext<'t>,
    data: &'t str,
) -> ParseResult<(Vec<Token<'t>>, usize)> {
    // Try compound tokens
    if let Some((tok, sz)) = read_compound_tokens(ctx, data)? {
        return Ok((vec![tok], sz));
    }

    // Detect indent
    if let Some(Token::NewLine(_)) = ctx.previous_token {
        if let Some((toks, count)) = try_read_indent(ctx, data)? {
            debug!(
                message = "read_next_tokens=>try_read_indent",
                read = ?count,
                first_char = ?data.chars().next(),
                position = ?ctx.position()
            );
            return Ok((toks, count));
        }
    }

    let next = match data.chars().next() {
        Some(c) => c,
        None => return Err(build_error(ctx, ParseError::UnexpectedEof)),
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
        '\n' | '\r' => read_newline(ctx, data)?,
        '$' => read_nodepath(ctx, data)?,
        '#' => read_comment(ctx, data)?,
        '\'' | '"' => read_string(ctx, data)?,
        '0'..='9' => read_number(ctx, data)?,
        ' ' | '\t' => read_whitespace(ctx, data)?,
        c if c.is_ascii_alphabetic() || c == '_' => read_identifier(ctx, data)?,
        other => return Err(build_error(ctx, ParseError::UnknownCharacter(other))),
    };

    Ok((vec![tok], length))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parser<'t>(
        method: impl Fn(&TokenReaderContext<'t>, &'t str) -> ParseResult<(Token<'t>, usize)>,
    ) -> impl Fn(&'t str) -> Token {
        move |s| {
            let mut ctx = TokenReaderContext::default();
            method(&mut ctx, s).unwrap().0
        }
    }

    fn parser_indent<'t>(
        method: impl Fn(
            &mut TokenReaderContext<'t>,
            &'t str,
        ) -> ParseResult<Option<(Vec<Token<'t>>, usize)>>,
    ) -> impl Fn(&mut TokenReaderContext<'t>, &'t str) -> Option<Vec<Token<'t>>> {
        move |ctx, s| method(ctx, s).unwrap().map(|(a, _)| a)
    }

    fn parser_newline<'t>(s: &'t str) -> Result<Token<'t>, (ParseError, ErrorContext)> {
        let ctx = TokenReaderContext::<'t>::default();
        read_newline(&ctx, s).map(|(t, _s)| t)
    }

    #[test]
    fn newline() {
        let run = parser_newline;

        assert_eq!(run("\n").unwrap(), Token::NewLine(NewLine::Lf));
        assert_eq!(run("\r\n").unwrap(), Token::NewLine(NewLine::CrLf));
        assert!(run("e").is_err());
    }

    #[test]
    fn string() {
        let run = parser(read_string);

        assert_eq!(
            run("'abcd'"),
            Token::Value(Value::String("abcd", QuoteMode::Single))
        );
        assert_eq!(
            run("\"abcd\""),
            Token::Value(Value::String("abcd", QuoteMode::Double))
        );
        assert_eq!(
            run("''"),
            Token::Value(Value::String("", QuoteMode::Single))
        );
        assert_eq!(
            run("\"\""),
            Token::Value(Value::String("", QuoteMode::Double))
        );
    }

    #[test]
    fn comment() {
        let run = parser(read_comment);

        assert_eq!(run("# abcd"), Token::Comment(" abcd"));
        assert_eq!(run("#abcd"), Token::Comment("abcd"));
    }

    #[test]
    fn number() {
        let run = parser(read_number);

        assert_eq!(run("1234"), Token::Value(Value::Int(1234)));
        assert_eq!(run("1234.0"), Token::Value(Value::Float("1234.0".into())));
        assert_eq!(run("1234.1"), Token::Value(Value::Float("1234.1".into())));
    }

    #[test]
    fn indent() {
        let mut ctx = TokenReaderContext::default();
        let run = parser_indent(try_read_indent);

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
        let run = parser_indent(try_read_indent);

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
        let run = parser(read_nodepath);

        assert_eq!(run("$A/B/C"), Token::NodePath("A/B/C", None));
        assert_eq!(
            run("$'A /B/C'"),
            Token::NodePath("A /B/C", Some(QuoteMode::Single))
        );
        assert_eq!(
            run("$\"A /B/C\""),
            Token::NodePath("A /B/C", Some(QuoteMode::Double))
        );
    }
}
