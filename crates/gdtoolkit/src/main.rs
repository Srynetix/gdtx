use color_eyre::Result;
use owo_colors::OwoColorize;

fn main() -> Result<()> {
    let status = gdtoolkit::run_cmdline()?;

    match status.exit_code() {
        0 => {
            if let Some(message) = status.message() {
                Ok(println!("{}", message.green()))
            } else {
                Ok(())
            }
        }
        n => {
            let message = status
                .message()
                .unwrap_or_else(|| "Unspecified error".into());
            let message = format!("{} (status code: {n})", message);
            Ok(eprintln!("{}", message.red()))
        }
    }
}
