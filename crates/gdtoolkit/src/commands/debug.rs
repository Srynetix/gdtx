use std::path::Path;

use crate::{
    args::{CodeToAstCommand, CodeToLexCommand, DebugCommand, DebugSubcommand, LexToCodeCommand},
    CommandStatus,
};

use color_eyre::{eyre::eyre, Result};
use gdtoolkit_gdscript_formatter::{GdScriptWriter, GdScriptWriterContext};
use gdtoolkit_gdscript_lexer::{
    GdScriptLexer, GdScriptLexerOutputFormat, GdScriptLexerOutputSerializer,
};
use gdtoolkit_gdscript_parser::{GdClassFormat, GdClassSerializer, GdScriptParser};

pub fn handle_debug_command(cmd: DebugCommand) -> Result<CommandStatus> {
    match cmd.debug_command {
        DebugSubcommand::CodeToLex(cmd) => handle_code_to_lex_command(cmd),
        DebugSubcommand::LexToCode(cmd) => handle_lex_to_code_command(cmd),
        DebugSubcommand::CodeToAst(cmd) => handle_code_to_ast_command(cmd),
    }
}

pub fn handle_code_to_lex_command(cmd: CodeToLexCommand) -> Result<CommandStatus> {
    let input_file = validate_file(&cmd.input)?;
    let input_content = std::fs::read_to_string(input_file)?;
    let lexer_output = GdScriptLexer::default().lex(&input_content)?;

    let mut buffer = Vec::<u8>::new();
    let lex_fmt_output = GdScriptLexerOutputFormat::from(&lexer_output);
    let lex_fmt_output_serializer = GdScriptLexerOutputSerializer::default();
    lex_fmt_output_serializer.serialize(lex_fmt_output, &mut buffer)?;

    write_buffer_to_output(buffer, cmd.output.as_deref())?;
    Ok(CommandStatus::success())
}

pub fn handle_lex_to_code_command(cmd: LexToCodeCommand) -> Result<CommandStatus> {
    let input_file = validate_file(&cmd.input)?;
    let input_content = std::fs::read_to_string(input_file)?;
    let lex_fmt_output_serializer = GdScriptLexerOutputSerializer::default();
    let lex_fmt_output = lex_fmt_output_serializer.deserialize(&input_content)?;

    let ctx = GdScriptWriterContext {
        indentation_type: lex_fmt_output.indentation_type,
        indentation_size: lex_fmt_output.indentation_size,
    };
    let mut buffer = Vec::<u8>::new();
    GdScriptWriter::default().write_tokens(&ctx, &mut buffer, lex_fmt_output.tokens)?;

    write_buffer_to_output(buffer, cmd.output.as_deref())?;
    Ok(CommandStatus::success())
}

pub fn handle_code_to_ast_command(cmd: CodeToAstCommand) -> Result<CommandStatus> {
    let input_file = validate_file(&cmd.input)?;
    let input_content = std::fs::read_to_string(input_file)?;
    let lexer_output = GdScriptLexer::default().lex(&input_content)?;
    let tokens_no_ws = lexer_output.parsable_tokens();

    let mut buffer = Vec::<u8>::new();
    let output = GdScriptParser::default().parse_top_level(&tokens_no_ws)?;
    let fmt = GdClassFormat {
        version: "0.1.0".into(),
        class: output,
    };
    GdClassSerializer::default().serialize(fmt, &mut buffer)?;

    write_buffer_to_output(buffer, cmd.output.as_deref())?;
    Ok(CommandStatus::success())
}

fn validate_file(path: &Path) -> Result<&Path> {
    if path.is_file() {
        Ok(path)
    } else {
        Err(eyre!(
            "Unsupported path '{}'. It should be an existing file.",
            path.as_os_str().to_string_lossy()
        ))
    }
}

fn write_buffer_to_output(content: Vec<u8>, path: Option<&Path>) -> Result<()> {
    let data = String::from_utf8(content)?;

    if let Some(path) = path {
        std::fs::write(path, data)?;
        Ok(())
    } else {
        Ok(println!("{}", data))
    }
}
