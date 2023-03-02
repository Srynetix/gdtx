// Thanks https://github.com/mitsuhiko/similar/blob/main/examples/terminal-inline.rs

use std::io::Write;

use owo_colors::{AnsiColors, DynColors, OwoColorize};
use similar::{ChangeTag, TextDiff};

struct Line(Option<usize>);

impl std::fmt::Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.0 {
            None => write!(f, "    "),
            Some(idx) => write!(f, "{:<4}", idx + 1),
        }
    }
}

pub fn check_string_differences(source: &str, output: &str) -> Option<Diff> {
    if source == output {
        None
    } else {
        Some(Diff::new(source, output))
    }
}

#[derive(Debug)]
pub struct Diff {
    source: String,
    output: String,
}

impl Diff {
    pub fn new(source: &str, output: &str) -> Self {
        Self {
            source: source.into(),
            output: output.into(),
        }
    }

    pub fn render<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        let diff = TextDiff::from_lines(&self.source, &self.output);

        for (idx, group) in diff.grouped_ops(3).iter().enumerate() {
            if idx > 0 {
                writeln!(writer, "{:-^1$}", "-", 80)?;
            }

            for op in group {
                for change in diff.iter_inline_changes(op) {
                    let (sign, color) = match change.tag() {
                        ChangeTag::Delete => ("-", DynColors::Ansi(AnsiColors::Red)),
                        ChangeTag::Insert => ("+", DynColors::Ansi(AnsiColors::Green)),
                        ChangeTag::Equal => (" ", DynColors::Ansi(AnsiColors::Default)),
                    };

                    write!(
                        writer,
                        "{}{} |{}",
                        Line(change.old_index()).dimmed(),
                        Line(change.new_index()).dimmed(),
                        sign.color(color).bold()
                    )?;

                    for (emphasized, value) in change.iter_strings_lossy() {
                        if emphasized {
                            write!(writer, "{}", value.color(color).on_bright_black())?;
                        } else {
                            write!(writer, "{}", value.color(color))?;
                        }
                    }

                    if change.missing_newline() {
                        writeln!(writer)?;
                    }
                }
            }
        }

        Ok(())
    }

    pub fn render_to_string(&self) -> String {
        let mut buffer = Vec::new();

        self.render(&mut buffer).unwrap();

        String::from_utf8_lossy(&buffer).into_owned()
    }
}
