use std::error::Error;

#[derive(Default)]
pub struct Modes {
    pub verbose: bool,
    pub quiet: bool,
}

pub async fn run(_modes: Modes, _args: &[String]) -> Result<(), Box<dyn Error>> {
    eprintln!("run");
    Ok(())
}
