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

pub enum Operation<'a> {
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

pub async fn command_stage(_config: Config, op: Operation<'_>) -> Result<(), Box<dyn Error>> {
    match op {
        Operation::Normal(_args) => todo!(),
        Operation::Short(_args) => todo!(),
        Operation::Count(_args) => todo!(),
        Operation::Create(_args) => todo!(),
        Operation::Edit(_args) => todo!(),
        Operation::List(_args) => todo!(),
        Operation::Reassociate(_args) => todo!(),
        Operation::Help | Operation::Version => panic!("help and version not here"),
    }
}
