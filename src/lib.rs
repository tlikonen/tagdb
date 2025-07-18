use std::error::Error;

pub struct Config {
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
    Emacs,
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

pub async fn command_stage(_config: Config, cmd: Cmd<'_>) -> Result<(), Box<dyn Error>> {
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
