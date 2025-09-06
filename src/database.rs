use crate::{Config, Format, PROGRAM_NAME};
use futures::TryStreamExt; // STREAM.try_next()
use sqlx::{Connection, Row, SqliteConnection, sqlite::SqliteConnectOptions};
use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    error::Error,
    fs,
    path::{Path, PathBuf},
};

const PROGRAM_DB_VERSION: i32 = 7;

// Seconds from 1900-01-01T00:00:00Z to 1970-01-01T00:00:00Z. That is,
// from the beginning of Common Lisp universal time to the beginning of
// UNIX time. The database uses timestamps in Common Lisp format because
// this program was initially implemented in the Common Lisp language.
pub const CL_TIME_EPOCH: i64 = 2208988800;

pub async fn list_matching_records(
    db: &mut SqliteConnection,
    tags: &[String],
) -> Result<Option<HashSet<i32>>, sqlx::Error> {
    let mut intersect = HashSet::with_capacity(10);
    let mut set = HashSet::with_capacity(10);
    let mut first = true;

    for tag in tags {
        let mut rows = sqlx::query(
            "SELECT j.record_id FROM record_tag AS j \
             LEFT JOIN tags AS t ON j.tag_id = t.id \
             WHERE t.name LIKE $1",
        )
        .bind(like_esc_wild(tag))
        .fetch(&mut *db);

        set.clear();
        while let Some(row) = rows.try_next().await? {
            let id: i32 = row.try_get("record_id")?;
            set.insert(id);
        }

        if first {
            intersect.clone_from(&set);
            first = false;
        } else {
            intersect = intersect.intersection(&set).cloned().collect();
        }
    }

    if intersect.is_empty() {
        Ok(None)
    } else {
        Ok(Some(intersect))
    }
}

pub struct Record {
    pub id: i32,
    pub created: i64,
    pub modified: i64,
    pub tags: Vec<String>,
    pub content: String,
}

pub async fn list_records(
    db: &mut SqliteConnection,
    record_ids: HashSet<i32>,
) -> Result<Vec<Record>, sqlx::Error> {
    let mut records = Vec::with_capacity(5);

    for id in record_ids {
        let mut tags = Vec::with_capacity(5);

        {
            let mut rows = sqlx::query(
                "SELECT t.name FROM record_tag AS j \
                 LEFT JOIN tags AS t ON j.tag_id = t.id \
                 WHERE j.record_id = $1",
            )
            .bind(id)
            .fetch(&mut *db);

            while let Some(row) = rows.try_next().await? {
                let tag: String = row.try_get("name")?;
                tags.push(tag);
            }
        }

        if !tags.is_empty() {
            let row = sqlx::query("SELECT created, modified, content FROM records WHERE id = $1")
                .bind(id)
                .fetch_one(&mut *db)
                .await?;

            records.push(Record {
                id,
                created: row.try_get("created")?,
                modified: row.try_get("modified")?,
                tags: {
                    tags.sort_by_key(|tag| tag.to_lowercase());
                    tags
                },
                content: row.try_get("content")?,
            });
        }
    }

    records.sort_by_key(|r| r.tags.join(" ").to_lowercase());
    Ok(records)
}

pub async fn list_tags(
    db: &mut SqliteConnection,
    mut tags: &[String],
) -> Result<HashMap<String, u64>, sqlx::Error> {
    let empty = &[String::new()];
    if tags.is_empty() {
        tags = empty;
    }

    let mut name_count = HashMap::<String, u64>::with_capacity(50);

    for tag in tags {
        let mut rows = sqlx::query(
            "SELECT count(t.id) AS count, t.name FROM tags AS t \
             JOIN record_tag AS j ON t.id = j.tag_id \
             WHERE t.name LIKE $1 GROUP BY t.name",
        )
        .bind(like_esc_wild(tag))
        .fetch(&mut *db);

        while let Some(row) = rows.try_next().await? {
            let name: String = row.try_get("name")?;
            let count: u64 = row.try_get("count")?;
            name_count.insert(name, count);
        }
    }

    Ok(name_count)
}

pub async fn new_record(
    db: &mut SqliteConnection,
    tags: &[String],
    content: &str,
) -> Result<(), sqlx::Error> {
    //let id = insert_record(db, content).await?;

    // let mut tag_ids = Vec::with_capacity(5);
    // for tag in tags {
    //     tag_ids.push(get_or_insert_tag(db, tag).await?);
    // }
    Ok(())
}

async fn insert_record(db: &mut SqliteConnection, content: &str) -> Result<i32, sqlx::Error> {
    let now = current_time();
    let row = sqlx::query(
        "INSERT INTO records (created, modified, content) \
         VALUES ($1, $2, $3) RETURNING id",
    )
    .bind(now)
    .bind(now)
    .bind(content)
    .fetch_one(&mut *db)
    .await?;

    let id: i32 = row.try_get("id")?;
    change_counter_add(db, 1).await?;
    Ok(id)
}

pub async fn connect(config: &mut Config) -> Result<SqliteConnection, Box<dyn Error>> {
    let path;

    if let Some(db) = &config.database {
        path = PathBuf::from(&db);
        if let Some(dirs) = path.parent() {
            fs::create_dir_all(dirs).map_err(|e| {
                format!(
                    "Couldn’t create database file “{}”: {}",
                    path.to_string_lossy(),
                    e.kind()
                )
            })?;
        }
    } else {
        let name = format!("{PROGRAM_NAME}.sqlite");
        path = xdg::BaseDirectories::new()
            .place_data_file(&name)
            .map_err(|e| format!("Couldn’t create database file “{name}”: {}", e.kind()))?;
    }

    let opts = SqliteConnectOptions::new()
        .filename(&path)
        .create_if_missing(true)
        .optimize_on_close(true, None);
    let mut db = SqliteConnection::connect_with(&opts).await?;
    init(&mut db, &path).await?;

    match &config.format {
        Some(format) if config.format_save => {
            sqlx::query("UPDATE maintenance SET value = $1 WHERE key = 'output format'")
                .bind(match format {
                    Format::Text { color: false } => "text",
                    Format::Text { color: true } => "text-color",
                    Format::OrgMode => "org-mode",
                })
                .execute(&mut db)
                .await?;
        }

        Some(_) => (),

        None => {
            let row = sqlx::query("SELECT value FROM maintenance WHERE key = 'output format'")
                .fetch_optional(&mut db)
                .await?;

            if let Some(value) = row {
                let format: &str = value.try_get("value")?;

                let fmt = match format {
                    "text" => Format::Text { color: false },
                    "text-color" => Format::Text { color: true },
                    "org-mode" => Format::OrgMode,
                    _ => Default::default(),
                };

                config.format = Some(fmt);
            }
        }
    }

    Ok(db)
}

async fn init(db: &mut SqliteConnection, path: &Path) -> Result<(), Box<dyn Error>> {
    let db_exists = sqlx::query(
        "SELECT 1 FROM sqlite_master \
         WHERE type = 'table' AND name = 'maintenance'",
    )
    .fetch_optional(&mut *db)
    .await?
    .is_some();

    let pathname = || path.to_string_lossy();

    if db_exists {
        let row = sqlx::query("SELECT value FROM maintenance WHERE key = 'database version'")
            .fetch_one(&mut *db)
            .await?;
        let version: i32 = row.try_get("value")?;

        match version.cmp(&PROGRAM_DB_VERSION) {
            Ordering::Equal => (),

            Ordering::Less => {
                eprintln!(
                    "Updating database file “{}” from version {version} to {PROGRAM_DB_VERSION}.",
                    pathname()
                );
                for v in (version + 1)..=(PROGRAM_DB_VERSION) {
                    update_db(db, v).await?;
                }
                vacuum(db).await?;
            }

            Ordering::Greater => Err(format!(
                "The database version in file “{}” is {version}\n\
                 but this program can only handle versions upto {PROGRAM_DB_VERSION}.\n\
                 Please update the program.",
                pathname()
            ))?,
        }
    } else {
        // Database objects don't exist. Create all.
        eprintln!("Preparing database file “{}”.", pathname());

        let mut ta = db.begin().await?;

        sqlx::query("PRAGMA auto_vacuum = FULL")
            .execute(&mut *ta)
            .await?;

        sqlx::query("CREATE TABLE maintenance (key TEXT UNIQUE, value INTEGER)")
            .execute(&mut *ta)
            .await?;

        sqlx::query("INSERT INTO maintenance (key, value) VALUES ('database version', $1)")
            .bind(PROGRAM_DB_VERSION)
            .execute(&mut *ta)
            .await?;

        sqlx::query("INSERT INTO maintenance (key, value) VALUES ('change counter', 0)")
            .execute(&mut *ta)
            .await?;

        sqlx::query("INSERT INTO maintenance (key, value) VALUES ('output format', 'text')")
            .execute(&mut *ta)
            .await?;

        // Myöhemmin ehkä kenttiin: NOT NULL
        sqlx::query(
            "CREATE TABLE records \
             (id INTEGER PRIMARY KEY, created INTEGER, modified INTEGER, content TEXT)",
        )
        .execute(&mut *ta)
        .await?;

        // Myöhemmin ehkä: name TEXT UNIQUE NOT NULL
        sqlx::query(
            "CREATE TABLE tags \
             (id INTEGER PRIMARY KEY, name TEXT UNIQUE)",
        )
        .execute(&mut *ta)
        .await?;

        sqlx::query(
            "CREATE TABLE record_tag \
             (record_id INTEGER NOT NULL REFERENCES records(id) ON DELETE CASCADE, \
             tag_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE, \
             PRIMARY KEY (record_id, tag_id))",
        )
        .execute(&mut *ta)
        .await?;

        sqlx::query("CREATE INDEX idx_record_tag_tag_id ON record_tag (tag_id)")
            .execute(&mut *ta)
            .await?;

        ta.commit().await?;
    }

    sqlx::query("PRAGMA foreign_keys = ON")
        .execute(&mut *db)
        .await?;

    sqlx::query("PRAGMA case_sensitive_like = ON")
        .execute(&mut *db)
        .await?;

    Ok(())
}

async fn update_db(db: &mut SqliteConnection, version: i32) -> Result<(), Box<dyn Error>> {
    match version {
        2 => {
            // Add --color option.
            let mut ta = db.begin().await?;

            sqlx::query("INSERT INTO maintenance (key, value) VALUES ('color', 0)")
                .execute(&mut *ta)
                .await?;

            sqlx::query("UPDATE maintenance SET value = 2 WHERE key = 'database version'")
                .execute(&mut *ta)
                .await?;

            ta.commit().await?;
        }

        3 => {
            // Use foreign keys in record_tag table.
            sqlx::query("PRAGMA foreign_keys = OFF")
                .execute(&mut *db)
                .await?;

            let mut ta = db.begin().await?;

            sqlx::query(
                "CREATE TABLE record_tag_v3 \
                 (record_id INTEGER REFERENCES records(id) ON DELETE CASCADE, \
                 tag_id INTEGER REFERENCES tags(id) ON DELETE CASCADE)",
            )
            .execute(&mut *ta)
            .await?;

            sqlx::query(
                "INSERT INTO record_tag_v3 \
                 SELECT record_id, tag_id FROM record_tag",
            )
            .execute(&mut *ta)
            .await?;

            sqlx::query("DROP TABLE record_tag")
                .execute(&mut *ta)
                .await?;
            sqlx::query("ALTER TABLE record_tag_v3 RENAME TO record_tag")
                .execute(&mut *ta)
                .await?;
            sqlx::query("UPDATE maintenance SET value = 3 WHERE key = 'database version'")
                .execute(&mut *ta)
                .await?;

            ta.commit().await?;
        }

        4 => {
            // Composite primary key for record_tag table.
            sqlx::query("PRAGMA foreign_keys = OFF")
                .execute(&mut *db)
                .await?;

            let mut ta = db.begin().await?;

            sqlx::query("ALTER TABLE record_tag RENAME TO record_tag_old")
                .execute(&mut *ta)
                .await?;

            sqlx::query(
                "CREATE TABLE record_tag \
                 (record_id INTEGER NOT NULL REFERENCES records(id) ON DELETE CASCADE, \
                 tag_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE, \
                 PRIMARY KEY (record_id, tag_id))",
            )
            .execute(&mut *ta)
            .await?;

            sqlx::query(
                "INSERT INTO record_tag \
                 SELECT record_id, tag_id FROM record_tag_old",
            )
            .execute(&mut *ta)
            .await?;

            sqlx::query("DROP TABLE record_tag_old")
                .execute(&mut *ta)
                .await?;

            sqlx::query("UPDATE maintenance SET value = 4 WHERE key = 'database version'")
                .execute(&mut *ta)
                .await?;

            ta.commit().await?;

            sqlx::query("PRAGMA foreign_keys = ON")
                .execute(&mut *db)
                .await?;
        }

        5 => {
            // auto_vacuum = FULL
            let mut ta = db.begin().await?;

            sqlx::query("UPDATE maintenance SET value = 5 WHERE key = 'database version'")
                .execute(&mut *ta)
                .await?;

            sqlx::query("PRAGMA auto_vacuum = FULL")
                .execute(&mut *ta)
                .await?;

            ta.commit().await?;
        }

        6 => {
            // Remove --color option and introduce --format (output format).
            let mut ta = db.begin().await?;

            let row = sqlx::query("SELECT value FROM maintenance WHERE key = 'color'")
                .fetch_one(&mut *ta)
                .await?;
            let color: i32 = row.try_get("value")?;

            if color == 1 {
                sqlx::query(
                    "INSERT INTO maintenance (key, value) VALUES ('output format', 'text-color')",
                )
                .execute(&mut *ta)
                .await?;
            } else {
                sqlx::query(
                    "INSERT INTO maintenance (key, value) VALUES ('output format', 'text')",
                )
                .execute(&mut *ta)
                .await?;
            }

            sqlx::query("DELETE FROM maintenance WHERE key = 'color'")
                .execute(&mut *ta)
                .await?;

            sqlx::query("UPDATE maintenance SET value = 6 WHERE key = 'database version'")
                .execute(&mut *ta)
                .await?;

            ta.commit().await?;
        }

        7 => {
            // Add index for record_tag table.
            let mut ta = db.begin().await?;

            sqlx::query("CREATE INDEX idx_record_tag_tag_id ON record_tag (tag_id)")
                .execute(&mut *ta)
                .await?;

            sqlx::query("UPDATE maintenance SET value = 7 WHERE key = 'database version'")
                .execute(&mut *ta)
                .await?;

            ta.commit().await?;
        }

        other => Err(format!(
            "Updating to database version {other} is not supported."
        ))?,
    }
    Ok(())
}

async fn vacuum(db: &mut SqliteConnection) -> Result<(), sqlx::Error> {
    sqlx::query("VACUUM").execute(&mut *db).await?;
    change_counter_reset(&mut *db).await?;
    Ok(())
}

async fn change_counter_reset(db: &mut SqliteConnection) -> Result<(), sqlx::Error> {
    sqlx::query("UPDATE maintenance SET value = 0 WHERE key = 'change counter'")
        .execute(&mut *db)
        .await?;
    Ok(())
}

async fn change_counter_add(db: &mut SqliteConnection, count: u32) -> Result<(), sqlx::Error> {
    sqlx::query("UPDATE maintenance SET value = value + $1 WHERE key = 'change counter'")
        .bind(count)
        .execute(&mut *db)
        .await?;
    Ok(())
}

pub async fn assert_write_access(db: &mut SqliteConnection) -> Result<(), Box<dyn Error>> {
    change_counter_add(db, 0).await.map_err(
        |_| "Couldn’t get write access to the database. It’s probably locked by another process.",
    )?;
    Ok(())
}

fn like_esc_wild(string: &str) -> String {
    let mut new = String::with_capacity(string.len() + 3);
    new.push('%');

    for c in string.chars() {
        match c {
            '%' | '_' | '\\' => {
                new.push('\\');
                new.push(c);
            }
            '*' => new.push('%'),
            _ => new.push(c),
        }
    }

    new.push('%');
    new
}

fn current_time() -> i64 {
    chrono::Utc::now().timestamp() + CL_TIME_EPOCH
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_like_esc_wild() {
        assert_eq!("%abcd%", like_esc_wild("abcd"));
        assert_eq!("%a\\%b\\_cd%", like_esc_wild("a%b_cd"));
        assert_eq!("%ab\\\\cd%", like_esc_wild("ab\\cd"));
        assert_eq!("%abcd%", like_esc_wild("abcd"));
        assert_eq!("%\\_\\%\\\\%", like_esc_wild("_%\\"));
        assert_eq!("%ab%cd%", like_esc_wild("ab*cd"));
    }
}
