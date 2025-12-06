use just_getopt::{Args, OptFlags, OptSpecs, OptValue};
use std::{error::Error, process::ExitCode};
use tagdb::*;

#[tokio::main]
async fn main() -> ExitCode {
    let args = OptSpecs::new()
        .option("quiet", "q", OptValue::None)
        .option("quiet", "quiet", OptValue::None)
        .option("verbose", "v", OptValue::None)
        .option("verbose", "verbose", OptValue::None)
        .option("utc", "utc", OptValue::None)
        .option("db", "db", OptValue::RequiredNonEmpty)
        .option("format", "format", OptValue::RequiredNonEmpty)
        .option("short", "s", OptValue::None)
        .option("short", "short", OptValue::None)
        .option("count", "n", OptValue::None)
        .option("count", "count", OptValue::None)
        .option("create", "c", OptValue::None)
        .option("create", "create", OptValue::None)
        .option("create-stdin", "cs", OptValue::None)
        .option("create-stdin", "create-stdin", OptValue::None)
        .option("edit", "e", OptValue::None)
        .option("edit", "edit", OptValue::None)
        .option("list", "l", OptValue::None)
        .option("list", "list", OptValue::None)
        .option("retag", "r", OptValue::None)
        .option("retag", "retag", OptValue::None)
        .option("help", "h", OptValue::None)
        .option("help", "help", OptValue::None)
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
    let mut config = {
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
                "text" => Some(Format::Text { color: false }),
                "text-color" => Some(Format::Text { color: true }),
                "org-mode" => Some(Format::OrgMode),
                _ => Err(format!("Invalid value for option “--format={value}”."))?,
            };
        }

        Config {
            short: args.option_exists("short"),
            verbose: args.option_exists("verbose"),
            quiet: args.option_exists("quiet"),
            utc: args.option_exists("utc"),
            database: args.options_value_last("db").cloned(),
            format,
            format_save,
        }
    };

    let selected_option = {
        let mut options = Vec::with_capacity(1);

        for o in [
            "short",
            "count",
            "create",
            "create-stdin",
            "edit",
            "list",
            "retag",
            "help",
            "version",
        ] {
            if args.option_exists(o) && !options.contains(&o) {
                options.push(o);
            }
        }

        match options.len() {
            0 => "normal",
            1 => options[0],
            _ => Err("Only one command option is allowed. Use option “-h” alone for help.")?,
        }
    };

    let command = match selected_option {
        "help" => {
            println!(
                include_str!("usage.txt"),
                program = tagdb::PROGRAM_NAME,
                database = tagdb::database_name()
            );
            return Ok(());
        }

        "version" => {
            println!(
                "{prg} v{ver}\n\
                 Author:  {author}\n\
                 License: {license}",
                prg = tagdb::PROGRAM_NAME,
                ver = tagdb::PROGRAM_VERSION,
                author = tagdb::PROGRAM_AUTHORS,
                license = tagdb::PROGRAM_LICENSE,
            );
            return Ok(());
        }

        "normal" | "short" => {
            if config.quiet && config.verbose {
                eprintln!("Note: Option “-q” is ignored when combined with “-v”.");
                config.quiet = false;
            }
            Cmd::Normal(Tags::try_from(&args.other)?)
        }

        "count" => Cmd::Count(Tags::try_from(&args.other)?),

        "list" => {
            if args.other.is_empty() {
                Cmd::List(None)
            } else {
                Cmd::List(Some(Tags::try_from(&args.other)?))
            }
        }

        "create" => Cmd::Create(Tags::try_from(&args.other)?),
        "create-stdin" => Cmd::CreateStdin(Tags::try_from(&args.other)?),

        _ => panic!("unexpected command"),
    };

    tagdb::command_stage(config, command).await
}
