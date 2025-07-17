use just_getopt::{Args, OptFlags, OptSpecs, OptValue};
use std::{error::Error, process::ExitCode};

static PROGRAM_NAME: &str = env!("CARGO_PKG_NAME");
static PROGRAM_VERSION: &str = env!("CARGO_PKG_VERSION");
static PROGRAM_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
static PROGRAM_LICENSE: &str = env!("CARGO_PKG_LICENSE");

#[tokio::main]
async fn main() -> ExitCode {
    let args = OptSpecs::new()
        .option("help", "h", OptValue::None)
        .option("version", "version", OptValue::None)
        .flag(OptFlags::PrefixMatchLongOptions)
        .getopt(std::env::args().skip(1));

    let mut error = false;

    for u in &args.unknown {
        eprintln!("Unknown option ”{u}”.");
        error = true;
    }

    for o in args.required_value_missing() {
        eprintln!("Option ”{}” requires a value.", o.id);
        error = true;
    }

    if error {
        eprintln!("Use option ”-h” for help.");
        return ExitCode::FAILURE;
    }

    if args.option_exists("help") {
        println!(
            "Usage: {PROGRAM_NAME} [options] [--] TAG ...\n\n{txt}",
            txt = include_str!("usage.txt")
        );
        return ExitCode::SUCCESS;
    }

    if args.option_exists("version") {
        println!(
            "{PROGRAM_NAME} v{PROGRAM_VERSION}\n\
             Author:  {PROGRAM_AUTHORS}\n\
             License: {PROGRAM_LICENSE}"
        );
        return ExitCode::SUCCESS;
    }

    match config_stage(args).await {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{e}");
            ExitCode::FAILURE
        }
    }
}

async fn config_stage(_args: Args) -> Result<(), Box<dyn Error>> {
    eprintln!("config_stage");
    Ok(())
}
