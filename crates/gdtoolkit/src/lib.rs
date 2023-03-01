mod args;
mod commands;
pub(crate) mod utils;

use std::borrow::Cow;

use args::Args;
use clap::Parser;
use color_eyre::Result;
use tracing_subscriber::{layer::SubscriberExt, EnvFilter, Registry};
use tracing_tree::HierarchicalLayer;

pub struct CommandStatus {
    message: Option<String>,
    exit_code: usize,
}

impl CommandStatus {
    pub fn success() -> Self {
        Self {
            message: None,
            exit_code: 0,
        }
    }

    pub fn success_with_message<T: Into<Cow<'static, str>>>(message: T) -> Self {
        Self {
            message: Some(message.into().into()),
            exit_code: 0,
        }
    }

    pub fn error<T: Into<Cow<'static, str>>>(code: usize, message: T) -> Self {
        Self {
            message: Some(message.into().into()),
            exit_code: code,
        }
    }

    pub fn message(&self) -> Option<String> {
        self.message.as_ref().map(Into::into)
    }

    pub fn exit_code(&self) -> usize {
        self.exit_code
    }
}

pub fn run_cmdline() -> Result<CommandStatus> {
    color_eyre::install()?;

    let env_layer = EnvFilter::from_default_env();
    let subscriber = Registry::default().with(env_layer).with(
        HierarchicalLayer::new(2)
            .with_bracketed_fields(true)
            .with_targets(true),
    );

    tracing::subscriber::set_global_default(subscriber).ok();

    let args: Args = Parser::parse();
    commands::handle_command(args.command)
}
