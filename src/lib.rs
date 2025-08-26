use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Input<'a> {
    data: &'a str,
    pos: usize,
}

impl<'a> Input<'a> {
    pub fn new(data: &'a str) -> Self {
        Self { data, pos: 0 }
    }

    // this way we have immutability. this also comes in handy when we don't have to worry about
    // many &mut references all around.
    pub fn advance(&self, n: usize) -> Self {
        Self {
            data: self.data,
            pos: self.pos + n,
        }
    }

    pub fn remaining(&self) -> &'a str {
        &self.data[self.pos..]
    }

    pub fn is_empty(&self) -> bool {
        self.pos >= self.data.len()
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub pos: usize,
}

pub type ParseResult<'a, T> = Result<(T, Input<'a>), ParseError>;

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parse error at pos {}: {}", self.pos, self.message)
    }
}
