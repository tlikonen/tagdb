mod database;
mod objects;
mod prelude;
mod print;

pub use crate::objects::{Cmd, Config, Format, Tags};
use crate::prelude::*;

pub static PROGRAM_NAME: &str = env!("CARGO_PKG_NAME");
pub static PROGRAM_VERSION: &str = env!("CARGO_PKG_VERSION");
pub static PROGRAM_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
pub static PROGRAM_LICENSE: &str = env!("CARGO_PKG_LICENSE");

const TAG_PREFIX_EDITOR: &str = "#";

pub async fn command_stage(mut config: Config, cmd: Cmd) -> Result<(), Box<dyn Error>> {
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
        Cmd::Retag(old_new) => cmd_retag(&mut db, old_new).await?,
    }

    database::vacuum_check(&mut db).await?;
    Ok(())
}

async fn cmd_normal(
    db: &mut SqliteConnection,
    config: Config,
    tags: Tags,
) -> Result<(), Box<dyn Error>> {
    let mut first = true;
    for record in find_records(db, &tags).await? {
        if first {
            first = false;
        } else {
            println!();
        }
        record.print(&config);
    }
    Ok(())
}

async fn find_records(
    db: &mut SqliteConnection,
    tags: &Tags,
) -> Result<Vec<Record>, Box<dyn Error>> {
    let ids = database::list_matching_records(db, tags).await?;
    match ids {
        Some(set) => {
            let records = database::list_records(db, set).await?;
            Ok(records)
        }
        None => Err("Records not found.")?,
    }
}

async fn cmd_count(db: &mut SqliteConnection, tags: Tags) -> Result<(), Box<dyn Error>> {
    let ids = database::list_matching_records(db, &tags).await?;
    match ids {
        Some(set) => println!("{}", set.len()),
        None => println!("0"),
    }
    Ok(())
}

async fn cmd_list(
    db: &mut SqliteConnection,
    maybetags: Option<Tags>,
) -> Result<(), Box<dyn Error>> {
    let name_count = database::list_tags(db, maybetags.as_ref()).await?;

    if name_count.is_empty() {
        Err("No tags found.")?;
    } else {
        let mut list: Vec<(String, u64)> = name_count.into_iter().collect();
        list.sort_by_key(|(name, _)| name.to_lowercase());
        let width = list.iter().map(|x| num_width(x.1)).max().unwrap_or(0);

        for (name, count) in list {
            println!("{count:width$} {name}");
        }
    }
    Ok(())
}

async fn cmd_create(db: &mut SqliteConnection, tags: Tags) -> Result<(), Box<dyn Error>> {
    let mut ta = db.begin().await?;
    database::assert_write_access(&mut ta).await?;

    let file = tmp_file()?;
    let path = file.path();
    let name = path.to_string_lossy();
    run_text_editor(&name)?;

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

async fn cmd_create_stdin(db: &mut SqliteConnection, tags: Tags) -> Result<(), Box<dyn Error>> {
    let mut ta = db.begin().await?;
    database::assert_write_access(&mut ta).await?;

    let buffer = io::read_to_string(io::stdin())?;
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

async fn cmd_edit(
    db: &mut SqliteConnection,
    config: Config,
    tags: Tags,
) -> Result<(), Box<dyn Error>> {
    let mut ta = db.begin().await?;

    let records = find_records(&mut ta, &tags).await?;
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

    let mut headers_ids = HashMap::<String, i32>::with_capacity(10);
    let mut ids_headers = HashMap::<i32, String>::with_capacity(10);

    {
        let mut header_id: usize = 1;
        let mut first = true;

        for record in records {
            if first {
                first = false;
            } else {
                writeln!(&mut file)?;
            }

            let id_line = record.editor_id_line(header_id, &config);
            record.write(&mut file, &id_line)?;

            let record_id = record.id;
            headers_ids.insert(id_line.clone(), record_id);
            ids_headers.insert(record_id, id_line);

            header_id += 1;
        }
    }

    let path = file.path();
    let name = path.to_string_lossy();

    'editor: loop {
        run_text_editor(&name)?;
        let buffer = fs::read_to_string(path)?;

        let mut header_id: Option<i32> = None;
        let mut tags = HashSet::<&str>::with_capacity(10);
        let mut lines: Vec<&str> = Vec::with_capacity(20);
        let mut records: Vec<RecordEditor> = Vec::with_capacity(10);

        let mut read_tags = false;

        for line in buffer.lines() {
            // Is this new record header?
            if let Some(new_id) = headers_ids.get(line) {
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
            Some(old_id) => records.push(RecordEditor {
                id: old_id,
                tags: prepare_tags(&tags),
                content: remove_empty_lines(&lines),
            }),

            None => {
                println!("No data found.");
                if return_to_editor()? {
                    continue 'editor;
                } else {
                    Err("Aborted.")?;
                }
            }
        }

        for record in records {
            print!(
                "{} – ",
                ids_headers.get(&record.id).expect("Id is not set.")
            );
            io::stdout().flush()?;

            if let Some(content) = record.content {
                let mut tags = None;

                if let Some(proposed_tags) = &record.tags {
                    match Tags::try_from(proposed_tags) {
                        Ok(t) => tags = Some(t),
                        Err(e) => {
                            println!("FAILED");
                            eprintln!("{e}");
                            if return_to_editor()? {
                                continue 'editor;
                            } else {
                                Err("Aborted.")?;
                            }
                        }
                    }
                }

                let updated = RecordUpdate {
                    id: record.id,
                    tags,
                    content,
                };

                if let Err(e) = updated.update(&mut ta).await {
                    println!("FAILED");
                    eprintln!("{e}");
                    if return_to_editor()? {
                        continue 'editor;
                    } else {
                        Err("Aborted.")?;
                    }
                }

                println!("Updated");
            } else {
                // Empty content. Delete the record.
                record.delete(&mut ta).await?;
                println!("Deleted");
            }
        }

        break 'editor;
    }

    database::delete_unused_tags(&mut ta).await?;
    ta.commit().await?;
    Ok(())
}

async fn cmd_retag(db: &mut SqliteConnection, old_new: Tags) -> Result<(), Box<dyn Error>> {
    let (old, new) = {
        let mut tags = old_new.iter();
        match (tags.next(), tags.next()) {
            (Some(o), Some(n)) => {
                if o == n {
                    Err("OLD and NEW tag can’t be the same.")?;
                }
                (Tags::try_from([o])?, Tags::try_from([n])?)
            }
            _ => Err("The retag command requires two tag names: OLD and NEW.")?,
        }
    };

    let mut ta = db.begin().await?;
    database::assert_write_access(&mut ta).await?;
    database::retag(&mut ta, &old, &new).await?;

    ta.commit().await?;
    Ok(())
}

fn return_to_editor() -> Result<bool, Box<dyn Error>> {
    loop {
        print!(
            "Press ENTER to return to text editor. Write “abort” to quit and cancel all changes: "
        );
        io::stdout().flush()?;

        let mut buffer = String::with_capacity(6);
        io::stdin().read_line(&mut buffer)?;

        match buffer.as_str() {
            "\n" => return Ok(true),
            "abort\n" => return Ok(false),
            _ => (),
        }
    }
}

fn tmp_file() -> Result<tempfile::NamedTempFile, String> {
    let tmp = tempfile::Builder::new()
        .prefix(&format!("{PROGRAM_NAME}-"))
        .suffix(".txt")
        .rand_bytes(6)
        .tempfile()
        .map_err(|_| "Couldn’t create a temporary file for the new record.")?;
    Ok(tmp)
}

fn run_text_editor(name: &str) -> Result<(), Box<dyn Error>> {
    use std::{env, process::Command};

    let editor = match env::var("EDITOR") {
        Ok(value) if !value.is_empty() => value,
        _ => Err("Couldn’t launch text editor: the EDITOR variable is unset.")?,
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

fn is_valid_tag_name(tag: &str) -> bool {
    !tag.is_empty() && tag.chars().all(|x| !x.is_whitespace())
}

fn split_tag_string(s: &str) -> impl Iterator<Item = &str> {
    s.split_whitespace()
}

fn num_width(mut num: u64) -> usize {
    let mut width = 1;
    while num / 10 > 0 {
        width += 1;
        num /= 10;
    }
    width
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

fn prepare_tags(tags: &HashSet<&str>) -> Option<Vec<String>> {
    if tags.is_empty() {
        None
    } else {
        Some(tags.iter().map(|x| x.to_string()).collect())
    }
}

pub fn database_name() -> String {
    format!("{PROGRAM_NAME}.sqlite")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_valid_tag_name_fn() {
        assert!(!is_valid_tag_name(""));
        assert!(!is_valid_tag_name(" "));
        assert!(!is_valid_tag_name("\t"));
        assert!(!is_valid_tag_name("\n"));
        assert!(!is_valid_tag_name("\r"));
        assert!(!is_valid_tag_name("abc "));
        assert!(!is_valid_tag_name("ab cd"));
        assert!(!is_valid_tag_name("ab\tab"));
        assert!(!is_valid_tag_name("ab\n"));

        assert!(is_valid_tag_name("a"));
        assert!(is_valid_tag_name("€ä"));
        assert!(is_valid_tag_name("–"));
    }

    #[test]
    fn num_width_fn() {
        assert_eq!(1, num_width(0));
        assert_eq!(1, num_width(1));
        assert_eq!(1, num_width(9));
        assert_eq!(2, num_width(10));
        assert_eq!(2, num_width(99));
        assert_eq!(3, num_width(100));
        assert_eq!(3, num_width(999));
        assert_eq!(4, num_width(1000));
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
