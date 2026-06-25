use crate::prelude::*;

const TAGS_MAX_WIDTH: usize = 70;
const TAG_PREFIX: &str = "# Tags:";

type FileBuf<'a> = BufWriter<&'a NamedTempFile>;

impl Record {
    pub fn print(&self, config: &Config, stream: &mut OutBuf) -> Result<()> {
        let format = match &config.format {
            Some(f) => f,
            None => &Default::default(),
        };

        match format {
            Format::Text { color } => self.print_text(stream, config, *color),
            Format::OrgMode => self.print_orgmode(stream, config),
        }
    }

    fn print_text(&self, stream: &mut OutBuf, config: &Config, color: bool) -> Result<()> {
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
            writeln!(
                stream,
                "{}# Created:  {}{}\n\
                 {}# Modified: {}{}{}",
                colors(GREEN),
                colors(CYAN),
                format_time(self.created, config.utc),
                colors(GREEN),
                colors(CYAN),
                format_time(self.modified, config.utc),
                colors(OFF)
            )?;
        }

        if !config.quiet || config.verbose {
            for line in into_lines(&self.tags, TAGS_MAX_WIDTH) {
                writeln!(
                    stream,
                    "{}{TAG_PREFIX} {}{line}{}",
                    colors(GREEN),
                    colors(YELLOW),
                    colors(OFF)
                )?;
            }
            writeln!(stream)?;
        }

        if config.short {
            if let Some(line) = self.content.lines().next() {
                writeln!(stream, "{line}")?;
            }
        } else {
            write!(stream, "{}", self.content)?;
        }

        Ok(())
    }

    fn print_orgmode(&self, stream: &mut OutBuf, config: &Config) -> Result<()> {
        let mut lines = self.content.lines();

        let first = lines.next();
        if let Some(line) = first {
            if line.starts_with("* ") {
                writeln!(stream, "{line}")?;
            } else {
                writeln!(stream, "* {line}")?;
            }
        }

        if config.verbose {
            writeln!(
                stream,
                "# Created:  {}\n\
                 # Modified: {}",
                format_time(self.created, config.utc),
                format_time(self.modified, config.utc),
            )?;
        }

        if !config.quiet || config.verbose {
            for line in into_lines(&self.tags, TAGS_MAX_WIDTH) {
                writeln!(stream, "{TAG_PREFIX} {line}")?;
            }
        }

        if !config.short {
            for line in lines {
                writeln!(
                    stream,
                    "{}{line}",
                    if is_org_header(line) { "*" } else { "" }
                )?;
            }
        }

        Ok(())
    }

    pub fn write(&self, file: &mut FileBuf, id_line: &str) -> Result<()> {
        writeln!(file, "{id_line}")?;

        for line in into_lines(&self.tags, TAGS_MAX_WIDTH) {
            writeln!(file, "{TAG_PREFIX_EDITOR} {line}")?;
        }

        write!(file, "\n{}", self.content)?;
        Ok(())
    }

    pub fn editor_id_line(&self, id: usize, config: &Config) -> String {
        format!(
            "# Record: {id}  Created: {}",
            format_time(self.created, config.utc)
        )
    }
}

impl Records {
    pub fn write(
        &self,
        file: &mut FileBuf,
        headers: &mut EditorHeaders,
        config: &Config,
    ) -> Result<()> {
        let mut first = true;

        for (header_id, record) in (1..).zip(self.iter()) {
            if first {
                first = false;
            } else {
                writeln!(file)?;
            }

            let id_line = record.editor_id_line(header_id, config);
            record.write(file, &id_line)?;
            headers.insert(record.id, &id_line);
        }
        Ok(())
    }
}

impl EditorRecords {
    pub fn parse(buffer: &str, headers: &EditorHeaders) -> Result<Self> {
        let mut header_id: Option<i32> = None;
        let mut tags = HashSet::<&str>::with_capacity(10);
        let mut lines: Vec<&str> = Vec::with_capacity(20);
        let mut records: Vec<RecordEditor> = Vec::with_capacity(10);

        let mut read_tags = false;

        for line in buffer.lines() {
            // Is this new record header?
            if let Some(new_id) = headers.get_id(line) {
                if let Some(old_id) = header_id {
                    records.push(RecordEditor {
                        id: old_id,
                        tags: prepare_tags(&tags),
                        content: remove_empty_lines(&lines),
                    });

                    tags.clear();
                    lines.clear();
                }
                header_id = Some(*new_id);
                read_tags = true;
                continue;
            } else if header_id.is_none() {
                continue;
            }

            if read_tags {
                if let Some(s) = line.strip_prefix(TAG_PREFIX_EDITOR) {
                    for tag in split_tag_string(s) {
                        tags.insert(tag);
                    }
                    continue;
                } else {
                    read_tags = false;
                }
            }

            lines.push(line);
        }

        // Store the last record.
        match header_id {
            Some(old_id) => {
                records.push(RecordEditor {
                    id: old_id,
                    tags: prepare_tags(&tags),
                    content: remove_empty_lines(&lines),
                });
                Ok(Self(records))
            }
            None => Err("No data found.".into()),
        }
    }
}

impl TagList {
    pub fn print(&self, stream: &mut OutBuf) -> Result<()> {
        let mut list: Vec<(&String, &u64)> = self.iter().collect();
        list.sort_by_key(|(name, _)| name.to_lowercase());
        for (name, count) in list {
            writeln!(
                stream,
                "{c:w$} {n}",
                c = count,
                w = self.num_width,
                n = name,
            )?;
        }
        Ok(())
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

pub fn run_text_editor(file: &Path) -> Result<()> {
    let name = file.as_os_str();

    let editor = match env::var("EDITOR") {
        Ok(value) if !value.is_empty() => value,
        _ => return Err("Couldn’t launch text editor: the EDITOR variable is unset.".into()),
    };

    match Command::new(&editor).arg(name).status() {
        Ok(status) if status.success() => Ok(()),

        Ok(status) => {
            let err = match status.code() {
                Some(code) => {
                    format!("Text editor process “{editor}” returned an error code {code}.")
                }
                None => format!("Text editor process “{editor}” was terminated."),
            };
            Err(err.into())
        }

        Err(_) => Err(format!(
            "Couldn’t launch text editor “{editor}”. Check the EDITOR variable."
        )
        .into()),
    }
}

fn split_tag_string(s: &str) -> impl Iterator<Item = &str> {
    s.split_whitespace()
}

fn prepare_tags(tags: &HashSet<&str>) -> Option<Vec<String>> {
    if tags.is_empty() {
        None
    } else {
        Some(tags.iter().map(|x| x.to_string()).collect())
    }
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

    #[test]
    fn split_tag_string_fn() {
        fn sts(s: &str) -> Vec<String> {
            split_tag_string(s).map(|x| x.to_string()).collect()
        }

        assert_eq!(Vec::<String>::new(), sts("   "));
        assert_eq!(Vec::<String>::new(), sts(""));
        assert_eq!(vec!["111"], sts("111"));
        assert_eq!(vec!["€€€"], sts("€€€"));
        assert_eq!(
            vec!["111", "222", "€€", "444"],
            sts("  111   222  €€  444  ")
        );
    }
}
