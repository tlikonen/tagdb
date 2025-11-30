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
    Text { color: bool },
    OrgMode,
}

impl Default for Format {
    fn default() -> Self {
        Format::Text { color: true }
    }
}

#[derive(Default)]
pub struct Record {
    pub id: Option<i32>,
    pub created: Option<i64>,
    pub modified: Option<i64>,
    pub tags: Option<Vec<String>>,
    pub content: Option<String>,
}

pub enum Cmd<'a> {
    Normal(&'a [String]),
    Count(&'a [String]),
    Create(&'a [String]),
    CreateStdin(&'a [String]),
    Edit(&'a [String]),
    List(&'a [String]),
    Retag(&'a [String]),
    Help,
    Version,
}
