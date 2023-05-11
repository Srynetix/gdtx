use std::fmt::Write;

use super::token::{NewLine, Token};

/// Compact view, used for debug purposes.
pub struct CompactTokenView<'a, 't>(pub &'a Token<'t>);

impl<'a, 't> std::fmt::Display for CompactTokenView<'a, 't> {
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

pub struct TokenView<'a>(pub &'a str);

impl<'a> std::fmt::Display for TokenView<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const MAX_ELEMS: usize = 20;

        f.write_char('"')?;

        for elem in self.0.chars().take(MAX_ELEMS) {
            match elem {
                '\n' => f.write_str("\\n")?,
                '\r' => f.write_str("\\r")?,
                c => f.write_char(c)?,
            }
        }

        if self.0.len() > MAX_ELEMS {
            f.write_char('…')?;
        }

        f.write_char('"')?;

        Ok(())
    }
}
