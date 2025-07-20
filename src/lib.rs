mod database;

use std::error::Error;

pub struct Config {
    pub short: bool,
    pub verbose: bool,
    pub quiet: bool,
    pub utc: bool,
    pub database: Option<String>,
    pub format: Option<Format>,
    pub format_save: bool,
}

pub enum Format {
    Text,
    TextColor,
    OrgMode,
}

pub enum Cmd<'a> {
    Normal(&'a [String]),
    Short(&'a [String]),
    Count(&'a [String]),
    Create(&'a [String]),
    Edit(&'a [String]),
    List(&'a [String]),
    Reassociate(&'a [String]),
    Help,
    Version,
}

pub async fn command_stage(config: Config, cmd: Cmd<'_>) -> Result<(), Box<dyn Error>> {
    unsafe {
        libc::umask(0o077);
    }

    let _db = database::connect(&config).await?;

    match cmd {
        Cmd::Normal(_args) => todo!(),
        Cmd::Short(_args) => todo!(),
        Cmd::Count(_args) => todo!(),
        Cmd::Create(_args) => todo!(),
        Cmd::Edit(_args) => todo!(),
        Cmd::List(_args) => todo!(),
        Cmd::Reassociate(_args) => todo!(),
        Cmd::Help | Cmd::Version => panic!("help and version not here"),
    }
}
