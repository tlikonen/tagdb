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

pub struct Record {
    pub id: i32,
    pub created: i64,
    pub modified: i64,
    pub tags: Vec<String>,
    pub content: String,
}

pub struct RecordNew {
    pub tags: Tags,
    pub content: String,
}

pub struct RecordEditor {
    pub id: i32,
    pub tags: Option<Vec<String>>,
    pub content: Option<String>,
}

pub struct RecordUpdate {
    pub id: i32,
    pub tags: Option<Tags>,
    pub content: String,
}

pub enum Cmd {
    Normal(Tags),
    Count(Tags),
    List(Option<Tags>),
    Create(Tags),
    CreateStdin(Tags),
    Edit(Tags),
    Retag(Tags),
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

        if !invalid.is_empty() {
            Err(format!("Invalid tag names: {invalid}."))?
        } else if tags.is_empty() {
            Err("No tags.")?
        } else {
            Ok(Self(tags))
        }
    }

    pub fn iter(&self) -> TagsIter<'_> {
        TagsIter {
            tags: self,
            index: 0,
        }
    }
}

pub struct TagsIter<'a> {
    tags: &'a Tags,
    index: usize,
}

impl<'a> Iterator for TagsIter<'a> {
    type Item = &'a String;

    fn next(&mut self) -> Option<Self::Item> {
        match self.tags.0.get(self.index) {
            None => None,
            value => {
                self.index += 1;
                value
            }
        }
    }
}

impl<'a> IntoIterator for &'a Tags {
    type Item = &'a String;
    type IntoIter = TagsIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

fn is_valid_tag_name(tag: &str) -> bool {
    !tag.is_empty() && tag.chars().all(|x| !x.is_whitespace())
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

        let empty = Vec::<String>::new();
        assert!(Tags::try_from(empty).is_err());
        assert!(Tags::try_from(["abc "]).is_err());
        assert!(Tags::try_from([" "]).is_err());
        assert!(Tags::try_from(["abc "]).is_err());
    }

    #[test]
    fn tags_iterator() {
        let tags = Tags::try_from(["a", "b"]).unwrap();
        let mut it = tags.iter();

        assert_eq!(Some("a"), it.next().map(|x| x.as_str()));
        assert_eq!(Some("b"), it.next().map(|x| x.as_str()));
        assert_eq!(None, it.next());
    }

    #[test]
    fn tags_for_loop() {
        let tags = Tags::try_from(["a", "b", "c"]).unwrap();

        let mut output = Vec::new();
        for tag in &tags {
            output.push(tag);
        }
        assert_eq!(vec!["a", "b", "c"], output);
    }

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
}
