use crate::{Config, database::Record};

impl Record {
    pub fn print(&self, _config: &Config) {
        println!(
            "{} {} {} {:?}\n{}",
            self.id, self.created, self.modified, self.tags, self.content
        );
    }
}
