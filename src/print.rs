use crate::{Config, Format, database::Record};
use chrono::{DateTime, Local, Utc};

impl Record {
    pub fn print(&self, config: &Config) {
        let format = match &config.format {
            Some(f) => f,
            None => &Default::default(),
        };

        match format {
            Format::Text { color } => self.print_text(config, *color),
            Format::OrgMode => todo!(),
        }
    }

    fn print_text(&self, config: &Config, color: bool) {
        const TAGS_MAX_WIDTH: usize = 70;

        const ESC: char = '\u{001B}';
        const GREEN: &str = "0;32";
        const YELLOW: &str = "0;33";
        const CYAN: &str = "0;36";
        const OFF: &str = "0";

        let colors = |code| {
            if color {
                format!("{ESC}[{code}m")
            } else {
                String::new()
            }
        };

        if config.verbose {
            println!(
                "{}# Created:  {}{}\n\
                 {}# Modified: {}{}{}",
                colors(GREEN),
                colors(CYAN),
                format_time(self.created, config.utc),
                colors(GREEN),
                colors(CYAN),
                format_time(self.modified, config.utc),
                colors(OFF)
            );
        }

        if !config.quiet || config.verbose {
            for line in into_lines(&self.tags, TAGS_MAX_WIDTH) {
                println!(
                    "{}# Tags: {}{}{}",
                    colors(GREEN),
                    colors(YELLOW),
                    line,
                    colors(OFF)
                );
            }
            println!();
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

fn into_lines<I, S>(words: I, max: usize) -> Vec<String>
where
    I: IntoIterator<Item = S>,
    S: ToString,
{
    let mut lines = Vec::with_capacity(2);
    let mut line = String::with_capacity(70);

    for word in words {
        let word = word.to_string();

        if line.is_empty() {
            line.push_str(&word);
        } else if line.chars().count() + word.chars().count() < max {
            line.push(' ');
            line.push_str(&word);
        } else {
            let l = line.len();
            lines.push(line);
            line = String::with_capacity(l);
            line.push_str(&word);
        }
    }

    lines.push(line);
    lines
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_into_lines() {
        for i in 0..8 {
            assert_eq!(
                vec!["€ka", "tøka", "kølmas"],
                into_lines(["€ka", "tøka", "kølmas"], i)
            );
        }

        for i in 8..15 {
            assert_eq!(
                vec!["€ka tøka", "kølmas"],
                into_lines(["€ka", "tøka", "kølmas"], i)
            );
        }

        assert_eq!(
            vec!["€ka tøka kølmas"],
            into_lines(["€ka", "tøka", "kølmas"], 15)
        );

        assert_eq!(vec![""], into_lines([""], 15));
    }
}
