mod database;
mod io;
mod objects;
mod prelude;

pub use crate::objects::{Cmd, Config, Format, Tag, Tags};
use crate::prelude::*;

pub static PROGRAM_NAME: &str = env!("CARGO_PKG_NAME");
pub static PROGRAM_VERSION: &str = env!("CARGO_PKG_VERSION");
pub static PROGRAM_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
pub static PROGRAM_LICENSE: &str = env!("CARGO_PKG_LICENSE");

const TAG_PREFIX_EDITOR: &str = "#";

pub async fn command_stage(mut config: Config, cmd: Cmd) -> ResultDE<()> {
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

async fn cmd_normal(db: &mut DBase, config: Config, tags: Tags) -> ResultDE<()> {
    let mut first = true;
    for record in &tags.find_records(db).await? {
        if first {
            first = false;
        } else {
            println!();
        }
        record.print(&config);
    }
    Ok(())
}

async fn cmd_count(db: &mut DBase, tags: Tags) -> ResultDE<()> {
    match tags.matching_record_ids(db).await? {
        Some(ids) => println!("{}", ids.count()),
        None => println!("0"),
    }
    Ok(())
}

async fn cmd_list(db: &mut DBase, maybetags: Option<Tags>) -> ResultDE<()> {
    let name_count = database::list_tags(db, maybetags.as_ref()).await?;

    if name_count.is_empty() {
        Err("No tags found.")?;
    } else {
        name_count.print();
    }
    Ok(())
}

async fn cmd_create(db: &mut DBase, tags: Tags) -> ResultDE<()> {
    let mut ta = db.begin().await?;
    database::assert_write_access(&mut ta).await?;

    let file = tmp_file()?;
    let path = file.path();
    let name = path.to_string_lossy();
    io::run_text_editor(&name)?;

    let buffer = fs::read_to_string(path)?;
    let lines: Vec<&str> = buffer.lines().collect();

    match remove_empty_lines(&lines) {
        Some(content) => {
            let new = RecordNew { tags, content };
            new.insert(&mut ta).await?;
        }
        None => Err("Empty file. Aborting.")?,
    }

    ta.commit().await?;
    Ok(())
}

async fn cmd_create_stdin(db: &mut DBase, tags: Tags) -> ResultDE<()> {
    let mut ta = db.begin().await?;
    database::assert_write_access(&mut ta).await?;

    let buffer = stdio::read_to_string(stdio::stdin())?;
    let lines: Vec<&str> = buffer.lines().collect();

    match remove_empty_lines(&lines) {
        Some(content) => {
            let new = RecordNew { tags, content };
            new.insert(&mut ta).await?;
        }
        None => Err("Empty content. Aborting.")?,
    }

    ta.commit().await?;
    Ok(())
}

async fn cmd_edit(db: &mut DBase, config: Config, tags: Tags) -> ResultDE<()> {
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
        io::run_text_editor(&name)?;
        let buffer = fs::read_to_string(path)?;

        let records = match EditorRecords::parse(&buffer, &headers) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("{e}");
                if return_to_editor()? {
                    continue 'editor;
                } else {
                    return Err("Aborted.".into());
                }
            }
        };

        for record in records.into_iter() {
            print!(
                "{} – ",
                headers.get_header(record.id).expect("Id is not set.")
            );
            stdio::stdout().flush()?;

            let mut error = false;
            let mut error_msg = String::new();
            match record.content {
                None => {
                    // Empty content. Delete the record.
                    record.delete(&mut ta).await?;
                    println!("Deleted");
                }

                Some(_) => match record.for_update() {
                    Ok(rec) => match rec.update(&mut ta).await {
                        Ok(_) => println!("Updated"),
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
                println!("FAILED");
                eprintln!("{error_msg}");
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

async fn cmd_retag(db: &mut DBase, old: Tag, new: Tag) -> ResultDE<()> {
    let mut ta = db.begin().await?;
    database::assert_write_access(&mut ta).await?;
    database::retag(&mut ta, &old, &new).await?;
    ta.commit().await?;
    Ok(())
}

fn return_to_editor() -> ResultDE<bool> {
    loop {
        print!(
            "Press ENTER to return to text editor. Write “abort” to quit and cancel all changes: "
        );
        stdio::stdout().flush()?;

        let mut buffer = String::with_capacity(6);
        stdio::stdin().read_line(&mut buffer)?;

        match buffer.as_str() {
            "\n" => return Ok(true),
            "abort\n" => return Ok(false),
            _ => (),
        }
    }
}

fn tmp_file() -> Result<NamedTempFile, String> {
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

    for (n, line) in lines.iter().enumerate() {
        if beginning {
            if is_empty_string(line) {
                skip = n + 1;
            } else {
                beginning = false;
                take = 1; // first non-empty line
            }
            continue;
        }

        if !is_empty_string(line) {
            take = n + 1 - skip;
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
