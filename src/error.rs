use std::fmt;

/// a parser returns a new reference to the input that the next parsers should parse from the
/// result also contains some data given by a parser if it was successful.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseError {
    CharMismatch,
    StringMismatch,
    ExpectedWhitespace,
    ValueParserMismatch,
    ExpectedAtLeastOne,
    ExpectedElementAfterSeparator,
    ExpectedAtLeastOneDigit,
    ExpectedDigitsAfterSign,
    NoAlternativeMatched,
    NoProgress,
    NotEnoughInputToTake,
    InvalidTakeBoundary,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = match self {
            ParseError::CharMismatch => "char mismatch",
            ParseError::StringMismatch => "string mismatch",
            ParseError::ExpectedWhitespace => "expected at least one whitespace character",
            ParseError::ValueParserMismatch => "didnt match value parser",
            ParseError::ExpectedAtLeastOne => "could not parse once",
            ParseError::ExpectedElementAfterSeparator => "expected element after separator",
            ParseError::ExpectedAtLeastOneDigit => "expected at least one digit",
            ParseError::ExpectedDigitsAfterSign => "expected digits after sign",
            ParseError::NoAlternativeMatched => "no alternative matched",
            ParseError::NoProgress => "parser succeeded without consuming input",
            ParseError::NotEnoughInputToTake => "not enough input to take",
            ParseError::InvalidTakeBoundary => "take count did not fall on a valid input boundary",
        };

        f.write_str(message)
    }
}

impl std::error::Error for ParseError {}

pub type ParseResult<I, T> = Result<(I, T), ParseError>;
