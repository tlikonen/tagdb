mod database;
mod error;
mod inout;
mod objects;
mod prelude;

use crate::prelude::*;
pub use crate::{
    error::Result,
    objects::{Cmd, Config, Format, Tag, Tags},
};

pub static PROGRAM_NAME: &str = env!("CARGO_PKG_NAME");
pub static PROGRAM_VERSION: &str = env!("CARGO_PKG_VERSION");
pub static PROGRAM_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
pub static PROGRAM_LICENSE: &str = env!("CARGO_PKG_LICENSE");

const TAG_PREFIX_EDITOR: &str = "#";

pub async fn command_stage(mut config: Config, cmd: Cmd) -> Result<()> {
    unsafe {
        libc::umask(0o077);
    }

    let mut db = database::connect(&mut config).await?;

    match cmd {
        Cmd::Normal(tags) => cmd_normal(&mut db, config, tags).await?,
        Cmd::Count(tags) => cmd_count(&mut db, tags).await?,
        Cmd::List(maybetags) => cmd_list(&mut db, maybetags).await?,
        Cmd::Create(tags) => cmd_create(&mut db, tags).await?,
        Cmd::CreateStdin(tags) => cmd_create_stdin(&mut db, tags).await?,
        Cmd::Edit(tags) => cmd_edit(&mut db, config, tags).await?,
        Cmd::Retag(old, new) => cmd_retag(&mut db, old, new).await?,
    }

    database::vacuum_check(&mut db).await?;
    Ok(())
}

async fn cmd_normal(db: &mut DBase, config: Config, tags: Tags) -> Result<()> {
    let mut first = true;
    for record in tags.find_records(db).await?.iter() {
        if first {
            first = false;
        } else {
            stdout("\n");
        }
        record.print(&config);
    }
    Ok(())
}

async fn cmd_count(db: &mut DBase, tags: Tags) -> Result<()> {
    match tags.matching_record_ids(db).await? {
        Some(ids) => stdout(&format!("{}\n", ids.count())),
        None => stdout("0\n"),
    }
    Ok(())
}

async fn cmd_list(db: &mut DBase, maybetags: Option<Tags>) -> Result<()> {
    let name_count = database::list_tags(db, maybetags.as_ref()).await?;

    if name_count.is_empty() {
        return Err("No tags found.".into());
    } else {
        name_count.print();
    }
    Ok(())
}

async fn cmd_create(db: &mut DBase, tags: Tags) -> Result<()> {
    let mut ta = db.begin().await?;
    database::assert_write_access(&mut ta).await?;

    let file = tmp_file()?;
    let path = file.path();
    let name = path.to_string_lossy();
    inout::run_text_editor(&name)?;

    let buffer = fs::read_to_string(path)?;
    let lines: Vec<&str> = buffer.lines().collect();

    match remove_empty_lines(&lines) {
        Some(content) => {
            let new = RecordNew { tags, content };
            new.insert(&mut ta).await?;
        }
        None => return Err("Empty file. Aborting.".into()),
    }

    ta.commit().await?;
    Ok(())
}

async fn cmd_create_stdin(db: &mut DBase, tags: Tags) -> Result<()> {
    let mut ta = db.begin().await?;
    database::assert_write_access(&mut ta).await?;

    let buffer = io::read_to_string(io::stdin())?;
    let lines: Vec<&str> = buffer.lines().collect();

    match remove_empty_lines(&lines) {
        Some(content) => {
            let new = RecordNew { tags, content };
            new.insert(&mut ta).await?;
        }
        None => return Err("Empty content. Aborting.".into()),
    }

    ta.commit().await?;
    Ok(())
}

async fn cmd_edit(db: &mut DBase, config: Config, tags: Tags) -> Result<()> {
    let mut ta = db.begin().await?;

    let records = tags.find_records(&mut ta).await?;
    database::assert_write_access(&mut ta).await?;

    let mut file = tmp_file()?;

    let edit_message_seen = database::is_edit_message_seen(&mut ta).await?;

    if !edit_message_seen || config.verbose {
        writeln!(file, "{}", include_str!("editor.txt"))?;
        if !edit_message_seen {
            writeln!(
                file,
                "# The above message will not show next time unless -v option is used.\n"
            )?;
            database::set_edit_message_seen(&mut ta).await?;
        }
    }

    let mut headers = EditorHeaders::new();
    records.write(&mut file, &mut headers, &config)?;

    let path = file.path();
    let name = path.to_string_lossy();

    'editor: loop {
        inout::run_text_editor(&name)?;
        let buffer = fs::read_to_string(path)?;

        for record in EditorRecords::parse(&buffer, &headers)?.into_iter() {
            stdout(&format!(
                "{} – ",
                headers.get_header(record.id).expect("Id is not set.")
            ));
            io::stdout().flush()?;

            let mut error = false;
            let mut error_msg = String::new();
            match record.content {
                None => {
                    // Empty content. Delete the record.
                    record.delete(&mut ta).await?;
                    stdout("Deleted\n");
                }

                Some(_) => match record.for_update() {
                    Ok(rec) => match rec.update(&mut ta).await {
                        Ok(_) => stdout("Updated\n"),
                        Err(e) => {
                            error_msg = format!("{e}");
                            error = true;
                        }
                    },

                    Err(e) => {
                        error_msg = format!("{e}");
                        error = true;
                    }
                },
            }

            if error {
                stdout("FAILED\n");
                stderr(&format!("{error_msg}\n"));
                if return_to_editor()? {
                    continue 'editor;
                } else {
                    return Err("Aborted.".into());
                }
            }
        }

        break 'editor;
    }

    database::delete_unused_tags(&mut ta).await?;
    ta.commit().await?;
    Ok(())
}

async fn cmd_retag(db: &mut DBase, old: Tag, new: Tag) -> Result<()> {
    let mut ta = db.begin().await?;
    database::assert_write_access(&mut ta).await?;
    database::retag(&mut ta, &old, &new).await?;
    ta.commit().await?;
    Ok(())
}

fn return_to_editor() -> Result<bool> {
    let mut buffer = String::with_capacity(6);
    loop {
        stdout(
            "Press ENTER to return to text editor. Write “abort” to quit and cancel all changes: ",
        );
        io::stdout().flush()?;

        buffer.clear();
        io::stdin().read_line(&mut buffer)?;

        match buffer.as_str() {
            "\n" => return Ok(true),
            "abort\n" => return Ok(false),
            _ => (),
        }
    }
}

fn tmp_file() -> Result<NamedTempFile> {
    let tmp = tempfile::Builder::new()
        .prefix(&format!("{PROGRAM_NAME}-"))
        .suffix(".txt")
        .rand_bytes(6)
        .tempfile()
        .map_err(|_| "Couldn’t create a temporary file for the new record.")?;
    Ok(tmp)
}

fn is_empty_string(s: &str) -> bool {
    s.chars().all(|x| x.is_whitespace())
}

fn remove_empty_lines(lines: &Vec<&str>) -> Option<String> {
    let mut skip = 0;
    let mut take = 0;
    let mut beginning = true;

    for (n, line) in (1..).zip(lines) {
        if beginning {
            if is_empty_string(line) {
                skip = n;
            } else {
                beginning = false;
                take = 1; // first non-empty line
            }
            continue;
        }

        if !is_empty_string(line) {
            take = n - skip;
        }
    }

    if take > 0 {
        let mut new = String::with_capacity(200);
        for line in lines.iter().skip(skip).take(take) {
            new.push_str(line);
            new.push('\n');
        }
        Some(new)
    } else {
        None
    }
}

pub fn database_name() -> String {
    format!("{PROGRAM_NAME}.sqlite")
}

pub fn stdout(s: &str) {
    let _ = write!(io::stdout(), "{s}");
}

pub fn stderr(s: &str) {
    let _ = write!(io::stderr(), "{s}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn remove_empty_lines_fn() {
        assert!(remove_empty_lines(&vec!("   ", "     ", "     ")).is_none());
        assert_eq!("one\n", remove_empty_lines(&vec!("one")).unwrap());
        assert_eq!(
            "one\n",
            remove_empty_lines(&vec!("  ", "one", "  ")).unwrap()
        );
        assert_eq!(
            "one\ntwo\nthree\n",
            remove_empty_lines(&vec!("  ", "one", "two", "three", "  ", "  ")).unwrap()
        );
    }
}
