mod database;
mod print;

use sqlx::{Connection, SqliteConnection};
use std::{
    collections::{HashMap, HashSet},
    error::Error,
    fs,
    io::{self, Write},
};

static PROGRAM_NAME: &str = env!("CARGO_PKG_NAME");
const TAG_PREFIX: &str = "# Tags: ";

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
        Format::Text { color: false }
    }
}

#[derive(Default)]
struct Record {
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

pub async fn command_stage(mut config: Config, cmd: Cmd<'_>) -> Result<(), Box<dyn Error>> {
    unsafe {
        libc::umask(0o077);
    }

    let mut db = database::connect(&mut config).await?;

    match cmd {
        Cmd::Normal(tags) => cmd_normal(&mut db, config, tags).await,
        Cmd::Count(tags) => cmd_count(&mut db, tags).await,
        Cmd::List(tags) => cmd_list(&mut db, tags).await,
        Cmd::Create(tags) => cmd_create(&mut db, tags).await,
        Cmd::CreateStdin(tags) => cmd_create_stdin(&mut db, tags).await,
        Cmd::Edit(tags) => cmd_edit(&mut db, config, tags).await,
        Cmd::Retag(_tags) => todo!(),
        Cmd::Help | Cmd::Version => panic!("help and version must be handled earlier"),
    }
}

async fn cmd_normal(
    db: &mut SqliteConnection,
    mut config: Config,
    tags: &[String],
) -> Result<(), Box<dyn Error>> {
    assert_tag_names(tags)?;

    if config.quiet & config.verbose {
        eprintln!("Note: Option “-q” is ignored when combined with “-v”.");
        config.quiet = false;
    }

    let mut first = true;
    for record in find_records(db, tags).await? {
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
    tags: &[String],
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

async fn cmd_count(db: &mut SqliteConnection, tags: &[String]) -> Result<(), Box<dyn Error>> {
    assert_tag_names(tags)?;
    let ids = database::list_matching_records(db, tags).await?;
    match ids {
        Some(set) => println!("{}", set.len()),
        None => println!("0"),
    }
    Ok(())
}

async fn cmd_list(db: &mut SqliteConnection, tags: &[String]) -> Result<(), Box<dyn Error>> {
    if !tags.is_empty() {
        assert_tag_names(tags)?;
    }

    let name_count = database::list_tags(db, tags).await?;

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

async fn cmd_create(db: &mut SqliteConnection, tags: &[String]) -> Result<(), Box<dyn Error>> {
    assert_tag_names(tags)?;
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
            let record = Record {
                tags: Some(tags.to_vec()),
                content: Some(content),
                ..Default::default()
            };
            record.create(&mut ta).await?;
        }
        None => Err("Empty file. Aborting.")?,
    }

    ta.commit().await?;
    Ok(())
}

async fn cmd_create_stdin(
    db: &mut SqliteConnection,
    tags: &[String],
) -> Result<(), Box<dyn Error>> {
    assert_tag_names(tags)?;
    let mut ta = db.begin().await?;
    database::assert_write_access(&mut ta).await?;

    let buffer = io::read_to_string(io::stdin())?;
    let lines: Vec<&str> = buffer.lines().collect();

    match remove_empty_lines(&lines) {
        Some(content) => {
            let record = Record {
                tags: Some(tags.to_vec()),
                content: Some(content),
                ..Default::default()
            };
            record.create(&mut ta).await?;
        }
        None => Err("Empty content. Aborting.")?,
    }

    ta.commit().await?;
    Ok(())
}

async fn cmd_edit(
    db: &mut SqliteConnection,
    config: Config,
    tags: &[String],
) -> Result<(), Box<dyn Error>> {
    assert_tag_names(tags)?;
    let mut ta = db.begin().await?;

    let records = find_records(&mut ta, tags).await?;
    database::assert_write_access(&mut ta).await?;

    let mut file = tmp_file()?;

    let edit_message_seen = database::is_edit_message_seen(&mut ta).await?;

    if !edit_message_seen || config.verbose {
        writeln!(file, "{}", include_str!("editor-message.txt"))?;
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
        let mut id: usize = 1;
        let mut first = true;

        for record in records {
            if first {
                first = false;
            } else {
                writeln!(&mut file)?;
            }

            let id_line = record.editor_id_line(id, &config);
            record.write(&mut file, &id_line)?;

            let record_id = record.id.expect("Record ID not set");
            headers_ids.insert(id_line.clone(), record_id);
            ids_headers.insert(record_id, id_line);

            id += 1;
        }
    }

    let path = file.path();
    let name = path.to_string_lossy();

    'editor: loop {
        run_text_editor(&name)?;
        let buffer = fs::read_to_string(path)?;

        let mut id: Option<i32> = None;
        let mut tags = HashSet::<&str>::with_capacity(10);
        let mut lines: Vec<&str> = Vec::with_capacity(20);
        let mut records: Vec<Record> = Vec::with_capacity(10);

        let mut read_tags = false;

        for line in buffer.lines() {
            // Is this new record header?
            if let Some(new_id) = headers_ids.get(line).copied() {
                if let Some(old_id) = id {
                    records.push(Record {
                        id: Some(old_id),
                        tags: prepare_tags(&tags),
                        content: remove_empty_lines(&lines),
                        ..Default::default()
                    });

                    tags.clear();
                    lines.clear();
                }
                id = Some(new_id);
                read_tags = true;
                continue;
            } else if id.is_none() {
                continue;
            }

            if read_tags {
                if let Some(s) = line.strip_prefix(TAG_PREFIX) {
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
        match id {
            Some(old_id) => records.push(Record {
                id: Some(old_id),
                tags: prepare_tags(&tags),
                content: remove_empty_lines(&lines),
                ..Default::default()
            }),

            None => {
                println!("No data found.");
                if return_to_editor() {
                    continue 'editor;
                } else {
                    Err("Aborted.")?;
                }
            }
        }

        for record in &records {
            let id = record.id.as_ref().expect("Id is not set.");
            print!("{} – ", ids_headers.get(id).unwrap());
            io::stdout().flush().expect("Flushing stdout failed.");

            if let Some(_content) = &record.content {
                if let Some(tags) = &record.tags {
                    if let Err(e) = assert_tag_names(tags) {
                        println!("FAILED");
                        eprintln!("{e}");
                        if return_to_editor() {
                            continue 'editor;
                        } else {
                            Err("Aborted.")?;
                        }
                    }
                }

                if let Err(e) = record.edit(&mut ta).await {
                    println!("FAILED");
                    eprintln!("{e}");
                    if return_to_editor() {
                        continue 'editor;
                    } else {
                        Err("Aborted.")?;
                    }
                }
                println!("Updated");
                println!("Muokkaus ei tee vielä mitään.");
            } else {
                // Empty content. Delete the record.
                println!("Deleted");
                println!("Ei oikeasti poisteta vielä.");
            }
        }

        break 'editor;
    }

    ta.commit().await?;
    Ok(())
}

fn return_to_editor() -> bool {
    loop {
        let mut buffer = String::with_capacity(1);
        print!(
            "Press ENTER to return to text editor. Write “abort” to quit and cancel all changes: "
        );
        io::stdout().flush().expect("Flushing stdout failed.");

        if io::stdin().read_line(&mut buffer).is_err() {
            return true;
        }
        if buffer == "abort\n" {
            return false;
        } else if buffer == "\n" {
            return true;
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

fn assert_tag_names(tags: &[String]) -> Result<(), Box<dyn Error>> {
    if tags.is_empty() {
        Err("No tags. At least one tag is required.")?;
    }

    let mut invalid = String::new();

    for t in tags {
        if !is_valid_tag_name(t) {
            if !invalid.is_empty() {
                invalid.push_str(", ");
            }
            invalid.push('“');
            invalid.push_str(t);
            invalid.push('”');
        }
    }

    if invalid.is_empty() {
        Ok(())
    } else {
        Err(format!("Invalid tag names: {invalid}."))?
    }
}

fn is_valid_tag_name(tag: &str) -> bool {
    // This used to be Common Lisp's (and (graphic-char-p x) (not (eql
    // #\space x))). Maybe change it to !x.is_whitespace(). The change
    // requires a new database version. The version update must convert
    // tags' whitespace characters to valid characters like "_" and
    // print information for user about changed tag names.
    !tag.is_empty() && tag.chars().all(|c| !" \n\r".contains(c))
}

fn split_tag_string(s: &str) -> impl Iterator<Item = &str> {
    // Maybe convert this to split_whitespace(). This may requires
    // database update. See is_valid_tag_name.
    s.split(' ')
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_valid_tag_name_fn() {
        assert!(!is_valid_tag_name(""));
        assert!(!is_valid_tag_name(" "));
        assert!(is_valid_tag_name("\t"));
        assert!(!is_valid_tag_name("\n"));
        assert!(!is_valid_tag_name("\r"));
        assert!(!is_valid_tag_name("abc "));
        assert!(!is_valid_tag_name("ab cd"));
        assert!(is_valid_tag_name("ab\t"));
        assert!(!is_valid_tag_name("ab\n"));
        assert!(is_valid_tag_name("a"));
        assert!(is_valid_tag_name("€ä"));
        assert!(is_valid_tag_name("–"));
    }

    #[test]
    fn assert_tag_names_fn() {
        fn atn(tags: impl IntoIterator<Item = impl ToString>) -> Result<(), Box<dyn Error>> {
            let vec = tags
                .into_iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>();
            assert_tag_names(&vec)
        }

        assert!(atn(["a", "ab", "öljyä", "–fas", "ab\tcd"]).is_ok());
        assert!(atn(Vec::<String>::new()).is_err());
        assert!(atn([""]).is_err());
        assert!(atn(["", "a"]).is_err());
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
