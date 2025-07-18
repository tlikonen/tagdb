use just_getopt::{Args, OptFlags, OptSpecs, OptValue};
use std::{error::Error, process::ExitCode};
use tagdb::{Config, Format, Operation};

static PROGRAM_NAME: &str = env!("CARGO_PKG_NAME");
static PROGRAM_VERSION: &str = env!("CARGO_PKG_VERSION");
static PROGRAM_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
static PROGRAM_LICENSE: &str = env!("CARGO_PKG_LICENSE");

#[tokio::main]
async fn main() -> ExitCode {
    let args = OptSpecs::new()
        .option("quiet", "q", OptValue::None)
        .option("verbose", "v", OptValue::None)
        .option("utc", "utc", OptValue::None)
        .option("db", "db", OptValue::RequiredNonEmpty)
        .option("format", "format", OptValue::RequiredNonEmpty)
        .option("short", "s", OptValue::None)
        .option("count", "n", OptValue::None)
        .option("create", "c", OptValue::None)
        .option("edit", "e", OptValue::None)
        .option("list", "l", OptValue::None)
        .option("reassociate", "r", OptValue::None)
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

    match config_stage(args).await {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{e}");
            ExitCode::FAILURE
        }
    }
}

async fn config_stage(args: Args) -> Result<(), Box<dyn Error>> {
    let config = {
        let mut format = None;
        let mut format_save = false;

        if let Some(value) = args.options_value_last("format") {
            let prefix = match value.strip_suffix("/default") {
                Some(v) => {
                    format_save = true;
                    v
                }
                None => value,
            };

            format = match prefix {
                "text" => Some(Format::Text),
                "text-color" => Some(Format::TextColor),
                "emacs" => Some(Format::Emacs),
                _ => Err(format!("Invalid value for option “--format={value}”."))?,
            };
        }

        Config {
            verbose: args.option_exists("verbose"),
            quiet: args.option_exists("quiet"),
            utc: args.option_exists("utc"),
            database: args.options_value_last("db").cloned(),
            format,
            format_save,
        }
    };

    let mut commands = 0;
    let mut op = Operation::Normal(&args.other);

    if args.option_exists("short") {
        op = Operation::Short(&args.other);
        commands += 1;
    }

    if args.option_exists("count") {
        op = Operation::Count(&args.other);
        commands += 1;
    }

    if args.option_exists("create") {
        op = Operation::Create(&args.other);
        commands += 1;
    }

    if args.option_exists("edit") {
        op = Operation::Edit(&args.other);
        commands += 1;
    }

    if args.option_exists("list") {
        op = Operation::List(&args.other);
        commands += 1;
    }

    if args.option_exists("reassociate") {
        op = Operation::Reassociate(&args.other);
        commands += 1;
    }

    if args.option_exists("help") {
        op = Operation::Help;
        commands += 1;
    }

    if args.option_exists("version") {
        op = Operation::Version;
        commands += 1;
    }

    if commands > 1 {
        Err("Only one command option is allowed. Use option “-h” alone for help.")?;
    }

    match op {
        Operation::Help => {
            println!(
                "Usage: {PROGRAM_NAME} [options] [--] TAG ...\n\n{txt}",
                txt = include_str!("usage.txt")
            );
            Ok(())
        }

        Operation::Version => {
            println!(
                "{PROGRAM_NAME} v{PROGRAM_VERSION}\n\
                 Author:  {PROGRAM_AUTHORS}\n\
                 License: {PROGRAM_LICENSE}"
            );
            Ok(())
        }

        operation => tagdb::command_stage(config, operation).await,
    }
}
