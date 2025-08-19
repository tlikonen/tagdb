mod database;
mod print;

use crate::database::Record;
use sqlx::{Connection, SqliteConnection};
use std::error::Error;

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

pub enum Cmd<'a> {
    Normal(&'a [String]),
    Count(&'a [String]),
    Create(&'a [String]),
    Edit(&'a [String]),
    List(&'a [String]),
    Reassociate(&'a [String]),
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
        Cmd::Create(tags) => cmd_create(&mut db, config, tags).await,
        Cmd::Edit(_tags) => todo!(),
        Cmd::Reassociate(_tags) => todo!(),
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
        let width = list.iter().map(|x| num_width(x.1)).max().unwrap();

        for (name, count) in list {
            println!("{count:width$} {name}");
        }
    }
    Ok(())
}

async fn cmd_create(
    db: &mut SqliteConnection,
    mut _config: Config,
    tags: &[String],
) -> Result<(), Box<dyn Error>> {
    assert_tag_names(tags)?;
    let mut ta = db.begin().await?;
    database::assert_write_access(&mut ta).await?;
    // KESKEN: Tutkitaan, tuleeko standardisyötteestä tekstiä.
    // Jos tulee, muodostetaan tietue siitä. Muuten avataan
    // tekstieditori.
    create_and_edit_new_record(&mut ta, tags).await?;
    ta.commit().await?;
    Ok(())
}

async fn create_and_edit_new_record(
    _db: &mut SqliteConnection,
    _tags: &[String],
) -> Result<(), Box<dyn Error>> {
    // KESKEN
    Ok(())
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

fn num_width(mut num: u64) -> usize {
    let mut width = 1;
    while num / 10 > 0 {
        width += 1;
        num /= 10;
    }
    width
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_is_valid_tag_name() {
        assert_eq!(false, is_valid_tag_name(""));
        assert_eq!(false, is_valid_tag_name(" "));
        assert_eq!(true, is_valid_tag_name("\t"));
        assert_eq!(false, is_valid_tag_name("\n"));
        assert_eq!(false, is_valid_tag_name("abc "));
        assert_eq!(false, is_valid_tag_name("ab cd"));
        assert_eq!(true, is_valid_tag_name("ab\t"));
        assert_eq!(false, is_valid_tag_name("ab\n"));
        assert_eq!(true, is_valid_tag_name("a"));
        assert_eq!(true, is_valid_tag_name("€ä"));
        assert_eq!(true, is_valid_tag_name("–"));
    }

    #[test]
    fn t_assert_tag_names() {
        fn atn(tags: impl IntoIterator<Item = impl ToString>) -> Result<(), Box<dyn Error>> {
            let vec = tags
                .into_iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>();
            assert_tag_names(&vec)
        }

        assert_eq!(true, atn(["a", "ab", "öljyä", "–fas", "ab\tcd"]).is_ok());
        assert_eq!(false, atn(Vec::<String>::new()).is_ok());
        assert_eq!(false, atn([""]).is_ok());
        assert_eq!(false, atn(["", "a"]).is_ok());
    }

    #[test]
    fn t_num_width() {
        assert_eq!(1, num_width(0));
        assert_eq!(1, num_width(1));
        assert_eq!(1, num_width(9));
        assert_eq!(2, num_width(10));
        assert_eq!(2, num_width(99));
        assert_eq!(3, num_width(100));
        assert_eq!(3, num_width(999));
        assert_eq!(4, num_width(1000));
    }
}
