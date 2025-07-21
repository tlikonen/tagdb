use crate::{Config, Format, database::Record};
use chrono::{DateTime, Local, Utc};

impl Record {
    pub fn print(&self, config: &Config) {
        let format = match &config.format {
            Some(f) => f,
            None => &Default::default(),
        };

        match format {
            Format::Text => self.print_text(config),
            Format::TextColor => todo!(),
            Format::OrgMode => todo!(),
        }
    }

    fn print_text(&self, config: &Config) {
        if config.verbose {
            println!(
                "# Created:  {}\n\
                 # Modified: {}",
                format_time(self.created, config.utc),
                format_time(self.modified, config.utc)
            );
        }

        if !config.quiet || config.verbose {
            println!("# Tags: {:?}\n", self.tags);
        }

        if config.short {
            println!("{}", self.content.lines().next().unwrap_or("(empty)"));
        } else {
            print!("{}", self.content);
        }
    }
}

// Seconds from 1900-01-01T00:00:00Z to 1970-01-01T00:00:00Z. That is,
// from the beginning of Common Lisp universal time to the beginning of
// UNIX time. The database uses timestamps in Common Lisp format because
// this program was initially implemented in the Common Lisp language.
const CL_TIME_EPOCH: i64 = 2208988800;

fn format_time(ut: i64, utc: bool) -> String {
    // ut = Common Lisp universal time: seconds since
    // 1900-01-01T00:00:00Z.

    let dt_utc = match DateTime::from_timestamp(ut - CL_TIME_EPOCH, 0) {
        Some(v) => v,
        None => return "(time decoding error)".to_string(),
    };

    if utc {
        format!("{}Z", dt_utc.format("%Y-%m-%d %H:%M:%S"))
    } else {
        format!(
            "{}",
            DateTime::<Local>::from(dt_utc).format("%Y-%m-%d %H:%M:%S%:z")
        )
    }
}

fn current_time() -> i64 {
    Utc::now().timestamp() + CL_TIME_EPOCH
}
