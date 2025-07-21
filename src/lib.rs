mod database;
mod print;

use crate::database::Record;
use sqlx::SqliteConnection;
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
    Text,
    TextColor,
    OrgMode,
}

pub enum Cmd<'a> {
    Normal(&'a [String]),
    Short(&'a [String]),
    Count(&'a [String]),
    Create(&'a [String]),
    Edit(&'a [String]),
    List(&'a [String]),
    Reassociate(&'a [String]),
    Help,
    Version,
}

pub async fn command_stage(config: Config, cmd: Cmd<'_>) -> Result<(), Box<dyn Error>> {
    unsafe {
        libc::umask(0o077);
    }

    let mut db = database::connect(&config).await?;

    match cmd {
        Cmd::Normal(args) | Cmd::Short(args) => {
            assert_tag_names(args)?;
            print_records(&config, &mut db, args).await
        }

        Cmd::Count(args) => {
            assert_tag_names(args)?;
            count_records(&mut db, args).await
        }

        Cmd::Create(_args) => todo!(),
        Cmd::Edit(_args) => todo!(),
        Cmd::List(_args) => todo!(),
        Cmd::Reassociate(_args) => todo!(),
        Cmd::Help | Cmd::Version => panic!("help and version must be handled earlier"),
    }
}

async fn print_records(
    config: &Config,
    db: &mut SqliteConnection,
    tags: &[String],
) -> Result<(), Box<dyn Error>> {
    for record in find_records(db, tags).await? {
        record.print(config);
    }
    Ok(())
}

async fn count_records(db: &mut SqliteConnection, tags: &[String]) -> Result<(), Box<dyn Error>> {
    let ids = database::list_matching_records(db, tags).await?;
    match ids {
        Some(set) => println!("{}", set.len()),
        None => println!("0"),
    }
    Ok(())
}

async fn find_records(
    db: &mut SqliteConnection,
    tags: &[String],
) -> Result<Vec<Record>, Box<dyn Error>> {
    let record_ids = database::list_matching_records(db, tags).await?;
    match record_ids {
        Some(ids) => {
            let records = database::list_records(db, ids).await?;
            Ok(records)
        }
        None => Err("Records not found.")?,
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
}
