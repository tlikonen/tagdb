use crate::prelude::*;

pub type ResultDE<T> = Result<T, Box<dyn std::error::Error>>;

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
    Retag(Tag, Tag),
}

#[derive(PartialEq)]
pub struct Tag(String);

impl Tag {
    pub fn try_from(name: &str) -> Result<Self, String> {
        if is_valid_tag_name(name) {
            Ok(Self(name.to_string()))
        } else {
            Err(format!("Invalid tag name: “{name}”."))
        }
    }

    pub fn as_str(&self) -> &String {
        &self.0
    }
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
                if !tags.contains(&name) {
                    tags.push(name);
                }
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
}

pub struct VecIter<T> {
    vec: T,
    index: usize,
}

impl<'a> Iterator for VecIter<&'a Tags> {
    type Item = &'a String;

    fn next(&mut self) -> Option<Self::Item> {
        match self.vec.0.get(self.index) {
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
    type IntoIter = VecIter<&'a Tags>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            vec: self,
            index: 0,
        }
    }
}

fn is_valid_tag_name(tag: &str) -> bool {
    !tag.is_empty() && tag.chars().all(|x| !x.is_whitespace())
}

pub struct RecordIds(pub HashSet<i32>);

impl RecordIds {
    pub fn hash(&self) -> &HashSet<i32> {
        &self.0
    }

    pub fn count(&self) -> usize {
        self.hash().len()
    }
}

pub struct Records(pub Vec<Record>);

impl<'a> Iterator for VecIter<&'a Records> {
    type Item = &'a Record;

    fn next(&mut self) -> Option<Self::Item> {
        match self.vec.0.get(self.index) {
            None => None,
            value => {
                self.index += 1;
                value
            }
        }
    }
}

impl<'a> IntoIterator for &'a Records {
    type Item = &'a Record;
    type IntoIter = VecIter<&'a Records>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            vec: self,
            index: 0,
        }
    }
}

pub struct TagList {
    pub hash: HashMap<String, u64>,
    pub num_width: usize,
}

impl TagList {
    pub fn iter(&self) -> impl Iterator<Item = (&String, &u64)> {
        self.hash.iter()
    }

    pub fn is_empty(&self) -> bool {
        self.hash.is_empty()
    }
}

pub struct EditorHeaders {
    headers_ids: HashMap<String, i32>,
    ids_headers: HashMap<i32, String>,
}

impl EditorHeaders {
    pub fn new() -> Self {
        Self {
            headers_ids: HashMap::<String, i32>::with_capacity(10),
            ids_headers: HashMap::<i32, String>::with_capacity(10),
        }
    }

    pub fn insert(&mut self, record_id: i32, header_line: &str) {
        self.headers_ids.insert(header_line.to_string(), record_id);
        self.ids_headers.insert(record_id, header_line.to_string());
    }

    pub fn get_header(&self, record_id: i32) -> Option<&String> {
        self.ids_headers.get(&record_id)
    }

    pub fn get_id(&self, header_line: &str) -> Option<&i32> {
        self.headers_ids.get(header_line)
    }
}

pub struct EditorRecords(pub Vec<RecordEditor>);

impl EditorRecords {
    pub fn into_iter(self) -> impl Iterator<Item = RecordEditor> {
        self.0.into_iter()
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

        let empty = Vec::<String>::new();
        assert!(Tags::try_from(empty).is_err());
        assert!(Tags::try_from(["abc "]).is_err());
        assert!(Tags::try_from([" "]).is_err());
        assert!(Tags::try_from(["abc "]).is_err());
    }

    #[test]
    fn tags_iterator() {
        let tags = Tags::try_from(["a", "b"]).unwrap();
        let mut it = tags.into_iter();

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
