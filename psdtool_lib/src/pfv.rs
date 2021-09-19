
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::{BufRead, BufReader, Read};
use std::ops::Bound;

use itertools::Itertools;

#[derive(Debug)]
pub(crate) struct Pfv {
    members: HashMap<String, (bool, Vec<String>)>,
}

#[derive(Debug)]
pub enum PfvParseError {
    InvalidFormat,
    DuplicatedName(String),
    DuplicatedFilterName(String),
    UnknownIdentifier(String),
    IoError(std::io::Error),
}

impl Display for PfvParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PfvParseError::InvalidFormat => write!(f, "invalid pfv file format."),
            PfvParseError::DuplicatedName(name) => write!(f, "name {:?} is duplicated", name),
            PfvParseError::DuplicatedFilterName(name) => write!(f, "filter name {:?} is duplicated", name),
            PfvParseError::UnknownIdentifier(ident) => write!(f, "unknown identifier {:?} after '~'", ident),
            PfvParseError::IoError(e) => write!(f, "{}", e)
        }
    }
}

impl Error for PfvParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            PfvParseError::IoError(e) => Some(e),
            _ => None
        }
    }
}

impl From<std::io::Error> for PfvParseError {
    fn from(e: std::io::Error) -> Self {
        PfvParseError::IoError(e)
    }
}

impl Pfv {
    pub(crate) fn empty() -> Self {
        Pfv { members: Default::default() }
    }
    pub(crate) fn from_pfv_file(file: impl Read) -> Result<Self, PfvParseError> {
        let file = BufReader::new(file);
        let mut lines = file.lines();
        match lines.next() {
            Some(Ok(line))if line.trim() == "[PSDToolFavorites-v1]" => {}
            Some(Err(e)) => return Err(PfvParseError::IoError(e)),
            _ => return Err(PfvParseError::InvalidFormat),
        }
        let lines = lines.skip_while(|line| line.as_ref().map_or(false, |line| !line.trim().starts_with("//"))).peekable();
        let entries = lines.batching(|iter| {
            let fav_name = iter.next()?;
            let fav_name = match fav_name {
                Ok(fav_name) => fav_name,
                Err(err) => return Some(Err(err)),
            };
            let mut items = Vec::new();
            for line in iter.peeking_take_while(|line| line.as_ref().map_or(false, |line| !line.trim().starts_with("//"))) {
                match line {
                    Ok(line) => items.push(line),
                    Err(err) => return Some(Err(err))
                }
            }
            Some(Ok((fav_name, items)))
        });
        let mut filters = BTreeMap::new();
        let mut result = HashMap::new();
        for entry in entries {
            let (fav_name, items) = entry?;
            let fav_name = fav_name.trim().strip_prefix("//").unwrap();
            let items = items.into_iter().map(|item| item.trim().to_string()).filter(|item| !item.is_empty());
            if let Some((fav_name, ident)) = fav_name.split_once("~") {
                match ident {
                    "filter" => {
                        if filters.insert(fav_name.to_string(), items.collect::<BTreeSet<_>>()).is_some() {
                            return Err(PfvParseError::DuplicatedFilterName(fav_name.to_string()));
                        }
                    }
                    _ => return Err(PfvParseError::UnknownIdentifier(ident.to_string())),
                }
            } else if result.insert(fav_name.to_string(), (false, items.collect::<Vec<_>>())).is_some() {
                return Err(PfvParseError::DuplicatedName(fav_name.to_string()));
            }
        }
        for (fav_name, (filtered, items)) in &mut result {
            if let Some((name, values)) = filters.range_mut::<str, _>((Bound::Unbounded, Bound::Included(fav_name.as_str()))).next_back() {
                if fav_name.starts_with(name) {
                    *filtered = true;
                    items.retain(|item| values.range::<str, _>((Bound::Unbounded, Bound::Included(item.as_str()))).next_back().map_or(false, |filter_value| item.starts_with(filter_value)));
                }
            }
        }

        Ok(Pfv { members: result })
    }
    pub(crate) fn get(&self, name: &str) -> Option<(bool, &[String])> {
        self.members.get(name).map(|(filtered, items)| (*filtered, items.as_slice()))
    }
}
