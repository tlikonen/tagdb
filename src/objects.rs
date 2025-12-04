use crate::prelude::*;

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
        Format::Text { color: true }
    }
}

#[derive(Default)]
pub struct Record {
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

#[derive(Debug, PartialEq)]
pub struct Tags(Vec<String>);

impl Tags {
    pub fn try_from<I, S>(names: I) -> Result<Self, String>
    where
        I: IntoIterator<Item = S>,
        S: ToString,
    {
        let mut invalid = String::new();
        let mut tags: Vec<String> = Vec::with_capacity(3);

        for name in names.into_iter().map(|x| x.to_string()) {
            if is_valid_tag_name(&name) {
                tags.push(name);
            } else {
                if !invalid.is_empty() {
                    invalid.push_str(", ");
                }
                invalid.push('“');
                invalid.push_str(&name);
                invalid.push('”');
            }
        }

        if invalid.is_empty() {
            Ok(Self(tags))
        } else {
            Err(format!("Invalid tag names: {invalid}."))?
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &String> {
        self.0.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn try_from() {
        assert_eq!(Ok(Tags(vec!("abc".to_string()))), Tags::try_from(["abc"]));
        assert_eq!(
            Ok(Tags(vec!("€äö".to_string(), "123".to_string()))),
            Tags::try_from(["€äö", "123"])
        );

        assert!(Tags::try_from(["abc "]).is_err());
        assert!(Tags::try_from([" "]).is_err());
        assert!(Tags::try_from(["abc "]).is_err());
    }
}
