use std::error::Error;

pub struct Modes {
    pub verbose: bool,
    pub quiet: bool,
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

pub async fn run(_modes: Modes, _op: Operation, _args: &[String]) -> Result<(), Box<dyn Error>> {
    eprintln!("run");
    Ok(())
}
