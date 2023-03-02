use std::path::PathBuf;

use clap::Parser;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    #[command(subcommand)]
    pub command: MainSubcommand,
}

#[derive(Debug, Parser)]
pub enum MainSubcommand {
    Fmt(FmtCommand),
    Debug(DebugCommand),
}

/// Auto-format GDScript code
#[derive(Debug, Parser)]
pub struct FmtCommand {
    /// Input path
    pub input: PathBuf,

    /// Only check formatting without showing output
    #[arg(short, long)]
    pub check: bool,
    /// Write formatting in each input file
    #[arg(short, long)]
    pub write: bool,
    /// File pattern to ignore
    #[arg(short, long)]
    pub ignore: Option<String>,
}

/// Misc. debug commands
#[derive(Debug, Parser)]
pub struct DebugCommand {
    #[command(subcommand)]
    pub debug_command: DebugSubcommand,
}

#[derive(Debug, Parser)]
pub enum DebugSubcommand {
    CodeToLex(CodeToLexCommand),
    LexToCode(LexToCodeCommand),
    CodeToAst(CodeToAstCommand),
}

/// Convert GDScript code to lex output
#[derive(Debug, Parser)]
pub struct CodeToLexCommand {
    /// Input path
    pub input: PathBuf,
    /// Output path (or stdout if missing)
    pub output: Option<PathBuf>,
}

/// Convert lex output to GDScript code
#[derive(Debug, Parser)]
pub struct LexToCodeCommand {
    /// Input path
    pub input: PathBuf,
    /// Output path (or stdout if missing)
    pub output: Option<PathBuf>,
}

/// Convert GDScript code to AST output
#[derive(Debug, Parser)]
pub struct CodeToAstCommand {
    /// Input path
    pub input: PathBuf,
    /// Output path (or stdout if missing)
    pub output: Option<PathBuf>,
}
