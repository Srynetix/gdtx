use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

use color_eyre::{eyre::Context, Result};
use gdtx_gdscript_formatter::{GdScriptWriter, GdScriptWriterContext};
use gdtx_gdscript_lexer::GdScriptLexer;
use owo_colors::OwoColorize;
use walkdir::WalkDir;

use crate::{
    args::FmtCommand,
    utils::diff::{check_string_differences, Diff},
    CommandStatus,
};

#[derive(Debug)]
pub enum FormatCheckOutput {
    Success(PathBuf),
    Error(PathBuf, Diff),
}

pub fn handle_fmt_command(cmd: FmtCommand) -> Result<CommandStatus> {
    if cmd.check {
        handle_format_check(cmd)
    } else {
        handle_format(cmd)
    }
}

fn handle_format(cmd: FmtCommand) -> Result<CommandStatus> {
    let paths = walk_dir(&cmd.input, cmd.ignore);
    for path in paths {
        let string_output = format_file_to_string(&path)?;

        if cmd.write {
            std::fs::write(path, string_output)?;
        } else {
            println!("{}", string_output)
        }
    }

    Ok(CommandStatus::success())
}

fn handle_format_check(cmd: FmtCommand) -> Result<CommandStatus> {
    let mut errors_found = false;
    let paths = walk_dir(&cmd.input, cmd.ignore);
    for path in paths {
        let res = check_format_for_file(&path)?;
        match res {
            FormatCheckOutput::Success(p) => {
                println!(
                    "{}",
                    format!("{} ok", p.as_os_str().to_string_lossy()).black()
                );
            }
            FormatCheckOutput::Error(p, diff) => {
                eprintln!(
                    "Error while formatting '{}'\n\n{}",
                    p.as_os_str().to_string_lossy(),
                    diff.render_to_string()
                );
                errors_found = true;
            }
        }
    }

    if errors_found {
        Ok(CommandStatus::error(
            1,
            "💥 Errors found during formatting.",
        ))
    } else {
        Ok(CommandStatus::success_with_message("All good."))
    }
}

fn generate_format_on_input(input_content: &str) -> Result<String> {
    let lexer_output = GdScriptLexer::default().lex(input_content)?;
    let ctx = GdScriptWriterContext::from(&lexer_output);

    let mut buffer = Vec::<u8>::new();
    GdScriptWriter::default().write_tokens(&ctx, &mut buffer, lexer_output.tokens())?;

    let s = String::from_utf8(buffer)?;
    Ok(s)
}

fn check_format_for_file(path: &Path) -> Result<FormatCheckOutput> {
    let input_content = std::fs::read_to_string(path)?;
    let output_string = generate_format_on_input(&input_content).with_context(|| {
        format!(
            "Could not parse input file {}",
            path.as_os_str().to_string_lossy()
        )
    })?;

    if let Some(diff) = check_string_differences(&input_content, &output_string) {
        Ok(FormatCheckOutput::Error(path.into(), diff))
    } else {
        Ok(FormatCheckOutput::Success(path.into()))
    }
}

fn format_file_to_string(path: &Path) -> Result<String> {
    let input_content = std::fs::read_to_string(path)?;
    let output_string = generate_format_on_input(&input_content).with_context(|| {
        format!(
            "Could not parse input file {}",
            path.as_os_str().to_string_lossy()
        )
    })?;

    Ok(output_string)
}

fn walk_dir(dir: &Path, ignore: Option<String>) -> Vec<PathBuf> {
    if dir.is_dir() {
        WalkDir::new(dir)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension() == Some(OsStr::new("gd")))
            .filter(|e| {
                if let Some(pat) = ignore.as_ref() {
                    !e.path().to_string_lossy().contains(pat)
                } else {
                    true
                }
            })
            .map(|m| m.into_path())
            .collect()
    } else {
        vec![dir.to_owned()]
    }
}
