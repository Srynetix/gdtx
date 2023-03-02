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
