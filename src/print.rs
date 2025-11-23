use {
    crate::{Config, Format, Record, TAG_PREFIX_EDITOR, database::CL_TIME_EPOCH},
    chrono::{DateTime, Local},
    std::{error::Error, io::Write},
};

const TAGS_MAX_WIDTH: usize = 70;
const TAG_PREFIX: &str = "# Tags:";

impl Record {
    pub fn print(&self, config: &Config) {
        let format = match &config.format {
            Some(f) => f,
            None => &Default::default(),
        };

        match format {
            Format::Text { color } => self.print_text(config, *color),
            Format::OrgMode => self.print_orgmode(config),
        }
    }

    fn print_text(&self, config: &Config, color: bool) {
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
                format_time(self.created.expect("Create date not set"), config.utc),
                colors(GREEN),
                colors(CYAN),
                format_time(self.modified.expect("Modified date not set"), config.utc),
                colors(OFF)
            );
        }

        if !config.quiet || config.verbose {
            for line in into_lines(self.tags.as_ref().expect("Tags missing"), TAGS_MAX_WIDTH) {
                println!(
                    "{}{TAG_PREFIX} {}{line}{}",
                    colors(GREEN),
                    colors(YELLOW),
                    colors(OFF)
                );
            }
            println!();
        }

        if config.short {
            if let Some(line) = self
                .content
                .as_ref()
                .expect("Content missing")
                .lines()
                .next()
            {
                println!("{line}");
            }
        } else {
            print!("{}", self.content.as_ref().expect("Content missing"));
        }
    }

    fn print_orgmode(&self, config: &Config) {
        let mut lines = self.content.as_ref().expect("Content missing").lines();

        let first = lines.next();
        if let Some(line) = first {
            if line.starts_with("* ") {
                println!("{line}");
            } else {
                println!("* {line}");
            }
        }

        if config.verbose {
            println!(
                "# Created:  {}\n\
                 # Modified: {}",
                format_time(self.created.expect("Create date not set"), config.utc),
                format_time(self.modified.expect("Modified date not set"), config.utc),
            );
        }

        if !config.quiet || config.verbose {
            for line in into_lines(self.tags.as_ref().expect("Tags missing"), TAGS_MAX_WIDTH) {
                println!("{TAG_PREFIX} {line}");
            }
        }

        if !config.short {
            for line in lines {
                println!("{}{line}", if is_org_header(line) { "*" } else { "" });
            }
        }
    }

    pub fn write(
        &self,
        file: &mut tempfile::NamedTempFile,
        id_line: &str,
    ) -> Result<(), Box<dyn Error>> {
        writeln!(file, "{id_line}")?;

        for line in into_lines(self.tags.as_ref().expect("Tags missing"), TAGS_MAX_WIDTH) {
            writeln!(file, "{TAG_PREFIX_EDITOR} {line}")?;
        }

        write!(
            file,
            "\n{}",
            self.content.as_ref().expect("Content missing")
        )?;
        Ok(())
    }

    pub fn editor_id_line(&self, id: usize, config: &Config) -> String {
        format!(
            "# Record: {id}  Created: {}",
            format_time(self.created.expect("Create date not set"), config.utc)
        )
    }
}

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

fn is_org_header(s: &str) -> bool {
    let mut level = 0;
    for c in s.chars() {
        match c {
            '*' => level += 1,
            ' ' => return level > 0,
            _ => return false,
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn into_lines_fn() {
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

    #[test]
    fn is_org_header_fn() {
        assert!(!is_org_header(""));
        assert!(!is_org_header(" "));
        assert!(!is_org_header("abc"));
        assert!(!is_org_header(" *"));
        assert!(!is_org_header(" * ab"));
        assert!(!is_org_header("*abc"));

        assert!(is_org_header("* abc"));
        assert!(is_org_header("** abc"));
        assert!(is_org_header("*** abc"));
        assert!(is_org_header("**** ä€–"));
    }
}
