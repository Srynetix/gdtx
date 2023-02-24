use crate::{args::MainSubcommand, CommandStatus};

mod fmt;

pub fn handle_command(command: MainSubcommand) -> color_eyre::Result<CommandStatus> {
    match command {
        MainSubcommand::Fmt(fmt) => fmt::handle_fmt_command(fmt),
    }
}
