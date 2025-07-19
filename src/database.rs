use crate::{Config, Format};
use sqlx::{Connection, Row, SqliteConnection, sqlite::SqliteConnectOptions};
use std::{
    cmp::Ordering,
    error::Error,
    fs,
    path::{Path, PathBuf},
};

//static DB_FILE: &str = env!("CARGO_PKG_NAME");
static DB_FILE: &str = "tagdb2";
const PROGRAM_DB_VERSION: i32 = 7;

pub async fn connect(config: &Config) -> Result<SqliteConnection, Box<dyn Error>> {
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
        let name = format!("{DB_FILE}.sqlite");
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

    if config.format_save {
        if let Some(format) = &config.format {
            sqlx::query("UPDATE maintenance SET value = $1 WHERE key = 'output format'")
                .bind(match format {
                    Format::Text => "text",
                    Format::TextColor => "text-color",
                    Format::OrgMode => "org-mode",
                })
                .execute(&mut db)
                .await?;
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
