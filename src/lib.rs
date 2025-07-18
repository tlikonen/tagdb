use std::error::Error;

pub struct Modes {
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

pub enum Operation {
    Normal,
    Short,
    Count,
    Create,
    Edit,
    List,
    Reassociate,
    Help,
    Version,
}

pub async fn command_stage(
    _modes: Modes,
    _op: Operation,
    _args: &[String],
) -> Result<(), Box<dyn Error>> {
    eprintln!("run");
    Ok(())
}
