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
}

/// Auto-format GDScript code
#[derive(Debug, Parser)]
pub struct FmtCommand {
    /// Input path
    pub input: PathBuf,
    /// Output path (or stdout if missing)
    pub output: Option<PathBuf>,

    /// Check formatting without writing
    #[arg(short, long)]
    pub check: bool,
    /// File pattern to ignore
    #[arg(short, long)]
    pub ignore: Option<String>,
}
