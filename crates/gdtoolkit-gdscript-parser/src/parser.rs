use std::fmt::Write;

use gdtoolkit_gdscript_lexer::{
    CompactTokenView, FloatType, IntType, Keyword, Punct, Token, Value,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Unexpected token: {0}")]
    UnexpectedToken(String),
    #[error("Unexpected EOF")]
    UnexpectedEof,
}

pub type Result<T> = core::result::Result<T, Error>;

struct TokenView<'a, 't>(&'a [Token<'t>]);

impl<'a, 't> std::fmt::Display for TokenView<'a, 't> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const MAX_ELEMS: usize = 10;

        for elem in self.0.iter().take(MAX_ELEMS) {
            f.write_fmt(format_args!("{}", CompactTokenView(elem)))?;
            f.write_str(", ")?;
        }

        if self.0.len() > MAX_ELEMS {
            f.write_char('…')?;
        }

        Ok(())
    }
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct GdType(String);

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct GdIdentifier(String);

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub enum GdDeclExtends {
    Type(GdType),
    String(String),
}

/// GDScript class.
#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct GdClass {
    name: Option<GdIdentifier>,
    decls: Vec<GdDecl>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub enum GdAstNode {
    Value(GdValue),
    Identifier(GdIdentifier),
    Type(GdType),
    Decls(Vec<GdDecl>),
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub enum GdDecl {
    Extends(GdDeclExtends),
    ClassName(GdType),
    Class(GdClass),
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub enum GdValue {
    Int(IntType),
    Float(FloatType),
    String(String),
    Boolean(bool),
}

/// GDScript parser.
#[derive(Default)]
pub struct GdScriptParser;

impl GdScriptParser {
    #[tracing::instrument(skip(self), fields(tokens = %TokenView(tokens)), level = "debug", ret)]
    fn parse_identifier<'t>(&self, tokens: &[Token<'t>]) -> Result<(GdIdentifier, usize)> {
        match tokens.first() {
            Some(Token::Identifier(i)) => Ok((GdIdentifier((*i).into()), 1)),
            Some(t) => Err(Error::UnexpectedToken(format!("{:?}", t))),
            None => Err(Error::UnexpectedEof),
        }
    }

    #[tracing::instrument(skip(self), fields(tokens = %TokenView(tokens)), level = "debug", ret)]
    fn parse_type(&self, tokens: &[Token]) -> Result<(GdType, usize)> {
        let mut cursor = 0;
        let mut value = String::new();

        loop {
            let (identifier, tok_count) = self.parse_identifier(&tokens[cursor..])?;
            value += &(identifier.0).to_owned();
            cursor += tok_count;

            match tokens.get(cursor) {
                Some(Token::Punct(Punct::Dot)) => {
                    value += ".";
                    cursor += 1;
                }
                _ => break,
            }
        }

        Ok((GdType(value), cursor))
    }

    #[tracing::instrument(skip(self), fields(tokens = %TokenView(tokens)), level = "debug", ret)]
    fn parse_value<'t>(&self, tokens: &[Token<'t>]) -> Result<(GdValue, usize)> {
        match tokens.first() {
            Some(Token::Value(Value::Int(i))) => Ok((GdValue::Int(*i), 1)),
            Some(Token::Value(Value::Float(f))) => Ok((GdValue::Float(f.to_float()), 1)),
            Some(Token::Value(Value::Boolean(b))) => Ok((GdValue::Boolean(*b), 1)),
            Some(Token::Value(Value::String(s, _))) => Ok((GdValue::String((*s).into()), 1)),
            Some(t) => Err(Error::UnexpectedToken(format!("{:?}", t))),
            None => Err(Error::UnexpectedEof),
        }
    }

    #[tracing::instrument(skip(self), fields(tokens = %TokenView(tokens)), level = "debug", ret)]
    fn ensure_keyword<'a, 't>(
        &self,
        keyword: Keyword,
        tokens: &'a [Token<'t>],
    ) -> Result<(Token<'t>, usize)> {
        match tokens.first() {
            Some(t @ Token::Keyword(k)) if *k == keyword => Ok((t.clone(), 1)),
            Some(t) => Err(Error::UnexpectedToken(format!("{:?}", t))),
            None => Err(Error::UnexpectedEof),
        }
    }

    #[tracing::instrument(skip(self), fields(tokens = %TokenView(tokens)), level = "debug", ret)]
    fn maybe_token<'a, 't>(
        &self,
        tok: Token<'t>,
        tokens: &'a [Token<'t>],
    ) -> Result<Option<(Token<'t>, usize)>> {
        match tokens.first() {
            Some(t) if *t == tok => Ok(Some((t.clone(), 1))),
            Some(_) => Ok(None),
            None => Err(Error::UnexpectedEof),
        }
    }

    #[tracing::instrument(skip(self), fields(tokens = %TokenView(tokens)), level = "debug", ret)]
    fn ensure_token<'a, 't>(
        &self,
        tok: Token<'t>,
        tokens: &'a [Token<'t>],
    ) -> Result<(Token<'t>, usize)> {
        match tokens.first() {
            Some(t) if *t == tok => Ok((t.clone(), 1)),
            Some(t) => Err(Error::UnexpectedToken(format!("{:?}", t))),
            None => Err(Error::UnexpectedEof),
        }
    }

    #[tracing::instrument(skip(self), fields(tokens = %TokenView(tokens)), level = "debug", ret)]
    fn ensure_indent<'a, 't>(&self, tokens: &'a [Token<'t>]) -> Result<(Token<'t>, usize)> {
        match tokens.first() {
            Some(t @ Token::Indent) => Ok((t.clone(), 1)),
            Some(t) => Err(Error::UnexpectedToken(format!("{:?}", t))),
            None => Err(Error::UnexpectedEof),
        }
    }

    #[tracing::instrument(skip(self), fields(tokens = %TokenView(tokens)), level = "debug", ret)]
    fn ensure_newline<'a, 't>(&self, tokens: &'a [Token<'t>]) -> Result<(Token<'t>, usize)> {
        match tokens.first() {
            Some(t @ Token::NewLine(_)) => Ok((t.clone(), 1)),
            Some(t) => Err(Error::UnexpectedToken(format!("{:?}", t))),
            None => Err(Error::UnexpectedEof),
        }
    }

    #[tracing::instrument(skip(self), fields(tokens = %TokenView(tokens)), level = "debug", ret)]
    fn ensure_punct<'a, 't>(&self, tokens: &'a [Token<'t>]) -> Result<(Token<'t>, usize)> {
        match tokens.first() {
            Some(t @ Token::Punct(_)) => Ok((t.clone(), 1)),
            Some(t) => Err(Error::UnexpectedToken(format!("{:?}", t))),
            None => Err(Error::UnexpectedEof),
        }
    }

    #[tracing::instrument(skip(self), fields(tokens = %TokenView(tokens)), level = "debug", ret)]
    fn parse_decl_extends<'t>(&self, tokens: &[Token<'t>]) -> Result<(GdDeclExtends, usize)> {
        let mut tokens = tokens;
        let mut count = 0;

        self.ensure_keyword(Keyword::Extends, tokens)
            .map(|(_, c)| {
                count += c;
                tokens = &tokens[c..]
            })?;

        match tokens.first() {
            Some(Token::Identifier(_)) => {
                let node = self.parse_type(tokens).map(|(gdtype, c)| {
                    count += c;
                    tokens = &tokens[c..];
                    gdtype
                })?;

                Ok((GdDeclExtends::Type(node), count))
            }
            Some(Token::Value(Value::String(s, _))) => {
                Ok((GdDeclExtends::String((*s).into()), count + 1))
            }
            Some(t) => Err(Error::UnexpectedToken(format!("{:?}", t))),
            None => Err(Error::UnexpectedEof),
        }
    }

    #[tracing::instrument(skip(self), fields(tokens = %TokenView(tokens)), level = "debug", ret)]
    fn parse_decl_class_name<'t>(&self, tokens: &[Token<'t>]) -> Result<(GdDecl, usize)> {
        let mut tokens = tokens;
        let mut count = 0;

        self.ensure_keyword(Keyword::ClassName, tokens)
            .map(|(_, c)| {
                count += c;
                tokens = &tokens[c..]
            })?;

        let gdtype = self.parse_type(tokens).map(|(t, c)| {
            count += c;
            tokens = &tokens[c..];
            t
        })?;

        Ok((GdDecl::ClassName(gdtype), count))
    }

    #[tracing::instrument(skip(self), fields(tokens = %TokenView(tokens)), level = "debug", ret)]
    fn parse_decl_class<'t>(&self, tokens: &[Token<'t>]) -> Result<(GdClass, usize)> {
        let mut tokens = tokens;
        let mut count = 0;

        self.ensure_keyword(Keyword::Class, tokens).map(|(_, c)| {
            count += c;
            tokens = &tokens[c..]
        })?;
        let class_name = self.parse_identifier(tokens).map(|(tok, c)| {
            count += c;
            tokens = &tokens[c..];
            tok
        })?;
        self.ensure_punct(tokens).map(|(_, c)| {
            count += c;
            tokens = &tokens[c..]
        })?;
        self.ensure_newline(tokens).map(|(_, c)| {
            count += c;
            tokens = &tokens[c..]
        })?;

        let mut cursor = 0;
        let mut indent_target = 0;
        let mut decls = vec![];

        loop {
            if cursor >= tokens.len() {
                break;
            }

            let remaining_tokens = &tokens[cursor..];
            match remaining_tokens.first() {
                Some(Token::Indent) => {
                    indent_target += 1;
                    cursor += 1;
                }
                Some(Token::Dedent) => {
                    indent_target -= 1;
                    cursor += 1;
                }
                Some(_) => {
                    if indent_target == 0 {
                        break;
                    }

                    match self.parse_decl(remaining_tokens) {
                        Ok((decl, sz)) => {
                            decls.push(decl);
                            cursor += sz;
                        }
                        Err(Error::UnexpectedToken(t)) => {
                            println!("Unexpected token {t:?}, continuing...");
                            cursor += 1;
                        }
                        Err(Error::UnexpectedEof) => break,
                    }
                }
                None => break,
            }
        }

        Ok((
            GdClass {
                name: Some(class_name),
                decls,
            },
            count + cursor,
        ))
    }

    #[tracing::instrument(skip(self), fields(tokens = %TokenView(tokens)), level = "debug", ret)]
    fn parse_decl<'t>(&self, tokens: &[Token<'t>]) -> Result<(GdDecl, usize)> {
        match tokens.first() {
            Some(Token::Keyword(Keyword::Extends)) => self
                .parse_decl_extends(tokens)
                .map(|(t, n)| (GdDecl::Extends(t), n + 1)),
            Some(Token::Keyword(Keyword::ClassName)) => {
                self.parse_decl_class_name(tokens).map(|(t, n)| (t, n + 1))
            }
            Some(Token::Keyword(Keyword::Class)) => self
                .parse_decl_class(tokens)
                .map(|(t, n)| (GdDecl::Class(t), n + 1)),
            Some(t) => Err(Error::UnexpectedToken(format!("{:?}", t))),
            None => Err(Error::UnexpectedEof),
        }
    }

    /// Parse tokens in a "top level" context (so a GDScript source file).
    #[tracing::instrument(skip(self, tokens), ret)]
    pub fn parse_top_level<'t>(&self, tokens: &[Token<'t>]) -> Result<GdClass> {
        let mut cursor = 0;
        let mut decls = vec![];

        loop {
            if cursor >= tokens.len() {
                break;
            }

            let remaining_tokens = &tokens[cursor..];
            match self.parse_decl(remaining_tokens) {
                Ok((decl, count)) => {
                    decls.push(decl);
                    cursor += count;
                }
                Err(Error::UnexpectedToken(t)) => {
                    println!("Unexpected token {t:?}, continuing...");
                    cursor += 1;
                }
                Err(Error::UnexpectedEof) => {
                    break;
                }
            }
        }

        Ok(GdClass { name: None, decls })
    }
}

#[cfg(test)]
mod tests {
    use crate::{GdClass, GdScriptParser};
    use gdtoolkit_gdscript_lexer::GdScriptLexer;

    use super::{GdDecl, GdDeclExtends, GdIdentifier, GdType};

    #[test]
    fn extends_and_classname() {
        gdtoolkit_tests::setup();

        let input = "# Hello\nextends Node\nclass_name Pouet\n\n";
        let lex_output = GdScriptLexer::default().lex(input).unwrap();

        let tokens = lex_output.parsable_tokens();
        let ast = GdScriptParser::default().parse_top_level(&tokens).unwrap();

        assert_eq!(
            ast,
            GdClass {
                name: None,
                decls: vec![
                    GdDecl::Extends(GdDeclExtends::Type(GdType("Node".into()))),
                    GdDecl::ClassName(GdType("Pouet".into()))
                ]
            }
        )
    }

    #[test]
    fn inner_class() {
        gdtoolkit_tests::setup();

        let input = &[
            "# Hello",
            "extends Node",
            "",
            "class _Inner:",
            "    extends Node",
            "",
            "    pouet",
            "class _Inner2:",
            "    extends Node2D",
            "    ",
            "    class _AlsoInner:",
            "        pass",
        ]
        .join("\n");

        let lex_output = GdScriptLexer::default().lex(input).unwrap();

        let tokens = lex_output.parsable_tokens();
        let ast = GdScriptParser::default().parse_top_level(&tokens).unwrap();

        assert_eq!(
            ast,
            GdClass {
                name: None,
                decls: vec![
                    GdDecl::Extends(GdDeclExtends::Type(GdType("Node".into()))),
                    GdDecl::Class(GdClass {
                        name: Some(GdIdentifier("_Inner".into())),
                        decls: vec![GdDecl::Extends(GdDeclExtends::Type(GdType("Node".into())))]
                    }),
                    GdDecl::Class(GdClass {
                        name: Some(GdIdentifier("_Inner2".into())),
                        decls: vec![
                            GdDecl::Extends(GdDeclExtends::Type(GdType("Node2D".into()))),
                            GdDecl::Class(GdClass {
                                name: Some(GdIdentifier("_AlsoInner".into())),
                                decls: vec![]
                            })
                        ]
                    })
                ]
            }
        )
    }
}
