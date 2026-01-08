use std::fmt;
use std::hash::Hash;

use memchr::memchr;
use rustc_hash::FxHashMap;

/// initial capacity for vectors to avoid reallocations
const VECTOR_INITIAL_CAPACITY: usize = 10;

/// Input is a lightweight abstraction over &str and &[u8].
pub trait Input: Copy {
    type Item: Copy;
    type Slice: Copy + AsRef<[u8]>;
    type Iter: Iterator<Item = (usize, Self::Item)>;

    fn len(self) -> usize;
    fn split_at(self, mid: usize) -> (Self, Self);
    fn first(self) -> Option<Self::Item>;
    fn starts_with(self, prefix: Self::Slice) -> bool;
    fn iter_indices(self) -> Self::Iter;
    fn item_len(item: Self::Item) -> usize;
    fn as_slice(self) -> Self::Slice;
    fn find_item(self, item: Self::Item) -> Option<usize>;
}

impl<'a> Input for &'a str {
    type Item = char;
    type Slice = &'a str;
    type Iter = std::str::CharIndices<'a>;

    fn len(self) -> usize {
        self.len()
    }

    fn split_at(self, mid: usize) -> (Self, Self) {
        self.split_at(mid)
    }

    fn first(self) -> Option<Self::Item> {
        self.chars().next()
    }

    fn starts_with(self, prefix: Self::Slice) -> bool {
        self.starts_with(prefix)
    }

    fn iter_indices(self) -> Self::Iter {
        self.char_indices()
    }

    fn item_len(item: Self::Item) -> usize {
        item.len_utf8()
    }

    fn as_slice(self) -> Self::Slice {
        self
    }

    fn find_item(self, item: Self::Item) -> Option<usize> {
        if item.is_ascii() {
            memchr(item as u8, self.as_bytes())
        } else {
            self.char_indices()
                .find(|(_, c)| *c == item)
                .map(|(idx, _)| idx)
        }
    }
}

impl<'a> Input for &'a [u8] {
    type Item = u8;
    type Slice = &'a [u8];
    type Iter = std::iter::Enumerate<std::iter::Copied<std::slice::Iter<'a, u8>>>;

    fn len(self) -> usize {
        self.len()
    }

    fn split_at(self, mid: usize) -> (Self, Self) {
        self.split_at(mid)
    }

    fn first(self) -> Option<Self::Item> {
        self.first().copied()
    }

    fn starts_with(self, prefix: Self::Slice) -> bool {
        self.starts_with(prefix)
    }

    fn iter_indices(self) -> Self::Iter {
        self.iter().copied().enumerate()
    }

    fn item_len(_: Self::Item) -> usize {
        1
    }

    fn as_slice(self) -> Self::Slice {
        self
    }

    fn find_item(self, item: Self::Item) -> Option<usize> {
        memchr(item, self)
    }
}

pub trait AsciiDigit: Copy {
    fn is_ascii_digit(self) -> bool;
}

impl AsciiDigit for char {
    fn is_ascii_digit(self) -> bool {
        char::is_ascii_digit(&self)
    }
}

impl AsciiDigit for u8 {
    fn is_ascii_digit(self) -> bool {
        u8::is_ascii_digit(&self)
    }
}

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
    NotEnoughInputToTake,
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
            ParseError::NotEnoughInputToTake => "not enough input to take",
        };

        f.write_str(message)
    }
}

impl std::error::Error for ParseError {}

pub type ParseResult<I, T> = Result<(I, T), ParseError>;

/// A Parser is a function that takes an input slice and returns a ParseResult.
pub trait Parser<I, Out> {
    fn parse(&self, input: I) -> ParseResult<I, Out>;
}

impl<I, F, Out> Parser<I, Out> for F
where
    F: Fn(I) -> ParseResult<I, Out>,
{
    fn parse(&self, input: I) -> ParseResult<I, Out> {
        self(input)
    }
}

/// multispace0 matches zero or more whitespace characters (space, tab, newline, carriage return)
#[inline(always)]
pub fn multispace0<I: Input>() -> impl Fn(I) -> ParseResult<I, I> {
    |input: I| {
        let slice = input.as_slice();
        let bytes = slice.as_ref();
        let mut end_pos = 0;

        while end_pos < bytes.len() {
            match bytes[end_pos] {
                b' ' | b'\t' | b'\n' | b'\r' => end_pos += 1,
                _ => break,
            }
        }

        let (matched, rest) = input.split_at(end_pos);
        Ok((rest, matched))
    }
}

/// multispace1 is like multispace0, but requires at least one whitespace character
#[inline(always)]
pub fn multispace1<I: Input>() -> impl Fn(I) -> ParseResult<I, I> {
    |input: I| {
        let (rest, matched) = multispace0()(input)?;
        if matched.len() == 0 {
            Err(ParseError::ExpectedWhitespace)
        } else {
            Ok((rest, matched))
        }
    }
}

/// Value returns a given value given to the function if the parser is successful, think of it as a
/// more ergonomic 'map'
#[inline(always)]
pub fn value<I, P, F, O, V>(p: P, val_fn: F) -> impl Fn(I) -> ParseResult<I, V>
where
    I: Input,
    P: Fn(I) -> ParseResult<I, O>,
    F: Fn() -> V,
{
    move |input: I| match p(input) {
        Ok((rest, _)) => Ok((rest, val_fn())),
        Err(_) => Err(ParseError::ValueParserMismatch),
    }
}

/// Tries to apply the parser `p`. If it fails, it returns the provided default value without
/// consuming any input. This will clone the default value.
pub fn or_default<I, P, O, D>(p: P, default: D) -> impl Fn(I) -> ParseResult<I, O>
where
    I: Input,
    P: Fn(I) -> ParseResult<I, O>,
    D: Fn() -> O,
{
    move |input: I| match p(input) {
        Ok((rest, res)) => Ok((rest, res)),
        Err(_) => Ok((input, default())),
    }
}

/// or tries to apply the first parser, if it fails, it tries the second parser
#[inline(always)]
pub fn or<I, P1, P2, O>(p1: P1, p2: P2) -> impl Fn(I) -> ParseResult<I, O>
where
    I: Input,
    P1: Fn(I) -> ParseResult<I, O>,
    P2: Fn(I) -> ParseResult<I, O>,
{
    move |input: I| match p1(input) {
        Ok((rest, res)) => Ok((rest, res)),
        Err(_) => p2(input),
    }
}

/// and applies two parsers in sequence and returns a tuple of their results
pub fn and<I, P1, P2, O1, O2>(p1: P1, p2: P2) -> impl Fn(I) -> ParseResult<I, (O1, O2)>
where
    I: Input,
    P1: Fn(I) -> ParseResult<I, O1>,
    P2: Fn(I) -> ParseResult<I, O2>,
{
    move |input: I| {
        let (rest, r1) = p1(input)?;
        let (rest, r2) = p2(rest)?;
        Ok((rest, (r1, r2)))
    }
}

/// take_while consumes input while the predicate is true. If the predicate is false at the start
/// of the input, an empty slice is returned.
pub fn take_while<I, F>(p: F) -> impl Fn(I) -> ParseResult<I, I>
where
    I: Input,
    F: Fn(I::Item) -> bool,
{
    move |input: I| {
        let mut end_pos = 0;

        for (idx, item) in input.iter_indices() {
            if !p(item) {
                break;
            }
            end_pos = idx + I::item_len(item);
        }

        let (matched, rest) = input.split_at(end_pos);
        Ok((rest, matched))
    }
}

/// take_until consumes input until the target item is found. The target item is not consumed. If
/// the target item is not found, the entire input is consumed.
pub fn take_until<I>(target: I::Item) -> impl Fn(I) -> ParseResult<I, I>
where
    I: Input,
    I::Item: PartialEq,
{
    move |input: I| {
        let end_pos = input.find_item(target).unwrap_or_else(|| input.len());

        let (matched, rest) = input.split_at(end_pos);
        Ok((rest, matched))
    }
}

/// char matches a specific item at the start of the input
#[inline(always)]
pub fn char<I>(expected: I::Item) -> impl Fn(I) -> ParseResult<I, I::Item>
where
    I: Input,
    I::Item: PartialEq,
{
    move |input: I| {
        if let Some(found) = input.first() {
            if found == expected {
                let (_, rest) = input.split_at(I::item_len(found));
                return Ok((rest, found));
            }
        }

        Err(ParseError::CharMismatch)
    }
}

/// string matches a specific slice at the start of the input
#[inline(always)]
pub fn string<I>(expected: I::Slice) -> impl Fn(I) -> ParseResult<I, I>
where
    I: Input,
{
    move |input: I| {
        if input.starts_with(expected) {
            let len = expected.as_ref().len();
            let (matched, rest) = input.split_at(len);
            Ok((rest, matched))
        } else {
            Err(ParseError::StringMismatch)
        }
    }
}

/// many applies a parser zero or more times and collects the results in a vector
pub fn many<I, P, O>(parser: P) -> impl Fn(I) -> ParseResult<I, Vec<O>>
where
    I: Input,
    P: Fn(I) -> ParseResult<I, O>,
{
    move |mut input: I| {
        let mut res = Vec::with_capacity(VECTOR_INITIAL_CAPACITY);

        loop {
            match parser(input) {
                Ok((rest, result)) => {
                    res.push(result);
                    input = rest;
                }
                Err(_) => break,
            }
        }

        Ok((input, res))
    }
}

/// map applies a function to the output of a parser
#[inline(always)]
pub fn map<I, P, O1, O2, F>(parser: P, f: F) -> impl Fn(I) -> ParseResult<I, O2>
where
    I: Input,
    P: Fn(I) -> ParseResult<I, O1>,
    F: Fn(O1) -> O2,
{
    move |input: I| {
        let (rest, result) = parser(input)?;
        Ok((rest, f(result)))
    }
}

/// many1 applies a parser one or more times and collects the results in a vector. If the parser
/// does not match at least once, an error is returned.
pub fn many1<I, P, O>(parser: P) -> impl Fn(I) -> ParseResult<I, Vec<O>>
where
    I: Input,
    P: Fn(I) -> ParseResult<I, O>,
{
    move |mut input: I| {
        let mut res = Vec::with_capacity(VECTOR_INITIAL_CAPACITY);
        match parser(input) {
            Ok((rest, result)) => {
                res.push(result);

                loop {
                    match parser(rest) {
                        Ok((rest, result)) => {
                            res.push(result);
                            input = rest;
                        }
                        Err(_) => break,
                    }
                }

                Ok((input, res))
            }
            Err(_) => Err(ParseError::ExpectedAtLeastOne),
        }
    }
}

/// separated applies two parsers in sequence, where the second parser is a separator. The results
/// of the first parser are collected into a vector. The separator parser is not included in the
/// results. If the first parser does not match at least once, an empty vector is returned
/// instead.
pub fn separated<I, P1, P2, O1, O2>(p1: P1, p2: P2) -> impl Fn(I) -> ParseResult<I, Vec<O1>>
where
    I: Input,
    P1: Fn(I) -> ParseResult<I, O1>,
    P2: Fn(I) -> ParseResult<I, O2>,
{
    move |mut input: I| {
        let mut res = Vec::with_capacity(VECTOR_INITIAL_CAPACITY);

        match p1(input) {
            Ok((rest, result)) => {
                res.push(result);
                input = rest;
            }
            Err(_) => {
                return Ok((input, res));
            }
        }

        loop {
            match p2(input) {
                Ok((rest, _)) => {
                    input = rest;
                    match p1(input) {
                        Ok((rest, result)) => {
                            res.push(result);
                            input = rest;
                        }
                        Err(_) => {
                            return Err(ParseError::ExpectedElementAfterSeparator);
                        }
                    }
                }
                Err(_) => {
                    break;
                }
            }
        }
        Ok((input, res))
    }
}

/// double parses a signed floating point number from the input
pub fn double<I: Input>() -> impl Fn(I) -> ParseResult<I, f64> {
    |input: I| {
        let slice = input.as_slice();
        let bytes = slice.as_ref();
        if bytes.is_empty() {
            return Err(ParseError::ExpectedAtLeastOneDigit);
        }

        let mut result = 0f64;
        let mut pos = 0;
        let mut negative = false;

        if bytes[0] == b'-' {
            negative = true;
            pos = 1;
        }

        if pos >= bytes.len() {
            return Err(ParseError::ExpectedDigitsAfterSign);
        }

        while pos < bytes.len() {
            match bytes[pos] {
                b'0'..=b'9' => {
                    result = result * 10.0 + (bytes[pos] - b'0') as f64;
                    pos += 1;
                }
                b'.' => {
                    pos += 1;
                    let mut frac = 0.1;
                    while pos < bytes.len() {
                        match bytes[pos] {
                            b'0'..=b'9' => {
                                result += (bytes[pos] - b'0') as f64 * frac;
                                frac *= 0.1;
                                pos += 1;
                            }
                            _ => break,
                        }
                    }
                    break;
                }
                _ => break,
            }
        }

        if pos == if negative { 1 } else { 0 } {
            return Err(ParseError::ExpectedAtLeastOneDigit);
        }

        let final_result = if negative { -result } else { result };
        let (_, rest) = input.split_at(pos);
        Ok((rest, final_result))
    }
}

/// integer parses a signed integer from the input
pub fn integer<I: Input>() -> impl Fn(I) -> ParseResult<I, i64> {
    |input: I| {
        let slice = input.as_slice();
        let bytes = slice.as_ref();
        if bytes.is_empty() {
            return Err(ParseError::ExpectedAtLeastOneDigit);
        }

        let mut result = 0i64;
        let mut pos = 0;
        let mut negative = false;

        if bytes[0] == b'-' {
            negative = true;
            pos = 1;
        }

        if pos >= bytes.len() {
            return Err(ParseError::ExpectedDigitsAfterSign);
        }

        while pos < bytes.len() {
            match bytes[pos] {
                b'0'..=b'9' => {
                    result = result * 10 + (bytes[pos] - b'0') as i64;
                    pos += 1;
                }
                _ => break,
            }
        }

        if pos == if negative { 1 } else { 0 } {
            return Err(ParseError::ExpectedAtLeastOneDigit);
        }

        let final_result = if negative { -result } else { result };
        let (_, rest) = input.split_at(pos);
        Ok((rest, final_result))
    }
}

pub fn digit01<I>() -> impl Fn(I) -> ParseResult<I, I>
where
    I: Input,
    I::Item: AsciiDigit,
{
    take_while(|c: I::Item| c.is_ascii_digit())
}

/// delimited requires that all three parsers pass, but it returns the value from the second
/// function this is especially useful for example reading the content of a string which is
/// surrounded by "" and then being able to map to that value
pub fn delimited<I, P1, P2, P3, O1, O2, O3>(
    first_delim: P1,
    inner: P2,
    second_delim: P3,
) -> impl Fn(I) -> ParseResult<I, O2>
where
    I: Input,
    P1: Fn(I) -> ParseResult<I, O1>,
    P2: Fn(I) -> ParseResult<I, O2>,
    P3: Fn(I) -> ParseResult<I, O3>,
{
    move |input: I| {
        let (rest, _) = first_delim(input)?;
        let (rest, out) = inner(rest)?;
        let (rest, _) = second_delim(rest)?;
        Ok((rest, out))
    }
}

/// separated_pair applies three parsers in sequence, but only returns the results of the first
/// and third parser. This is useful for example when parsing key-value pairs where the key and
/// value are separated by a colon or equals sign.
pub fn separated_pair<I, P1, P2, P3, O1, O2, O3>(
    p1: P1,
    p2: P2,
    p3: P3,
) -> impl Fn(I) -> ParseResult<I, (O1, O3)>
where
    I: Input,
    P1: Fn(I) -> ParseResult<I, O1>,
    P2: Fn(I) -> ParseResult<I, O2>,
    P3: Fn(I) -> ParseResult<I, O3>,
{
    move |input: I| {
        let (rest, out1) = p1(input)?;
        let (rest, _) = p2(rest)?;
        let (rest, out3) = p3(rest)?;
        Ok((rest, (out1, out3)))
    }
}

/// separated_into_map is like separated, but it expects the first parser to return a tuple of key
/// and value. The results are collected into a HashMap. If the same key is found
/// multiple times, the last value is kept. The capacity_hint is used to initialize the
/// HashMap with a certain capacity to avoid reallocations.
pub fn separated_into_map<I, P1, P2, O, K, V>(
    p1: P1,
    p2: P2,
    capacity_hint: usize,
) -> impl Fn(I) -> ParseResult<I, FxHashMap<K, V>>
where
    I: Input,
    P1: Fn(I) -> ParseResult<I, (K, V)>,
    P2: Fn(I) -> ParseResult<I, O>,
    K: Hash + Eq,
{
    move |mut input: I| {
        let mut map = FxHashMap::with_capacity_and_hasher(capacity_hint, Default::default());
        match p1(input) {
            Ok((rest, (key, value))) => {
                map.insert(key, value);
                input = rest;
            }
            Err(_) => return Ok((input, map)),
        }

        loop {
            match p2(input) {
                Ok((rest, _)) => {
                    input = rest;
                    match p1(input) {
                        Ok((rest, (key, value))) => {
                            map.insert(key, value);
                            input = rest;
                        }
                        Err(_) => return Err(ParseError::ExpectedElementAfterSeparator),
                    }
                }
                Err(_) => break,
            }
        }
        Ok((input, map))
    }
}

/// separated1 is like separated, but requires at least one element
pub fn separated1<I, P1, P2, O1, O2>(p1: P1, p2: P2) -> impl Fn(I) -> ParseResult<I, Vec<O1>>
where
    I: Input,
    P1: Fn(I) -> ParseResult<I, O1>,
    P2: Fn(I) -> ParseResult<I, O2>,
{
    move |mut input: I| {
        let mut res = Vec::with_capacity(VECTOR_INITIAL_CAPACITY);
        let (rest, result) = p1(input)?;
        res.push(result);
        input = rest;

        loop {
            match p2(input) {
                Ok((rest, _)) => {
                    input = rest;
                    match p1(input) {
                        Ok((rest, result)) => {
                            res.push(result);
                            input = rest;
                        }
                        Err(_) => {
                            return Err(ParseError::ExpectedElementAfterSeparator);
                        }
                    }
                }
                Err(_) => break,
            }
        }
        Ok((input, res))
    }
}

#[macro_export]
macro_rules! alt {
    ($first:expr $(, $rest:expr)* $(,)?) => {
        |input| {
            match ($first)(input) {
                Ok(result) => Ok(result),
                Err(_) => {
                    $(
                        match ($rest)(input) {
                            Ok(result) => return Ok(result),
                            Err(_) => {}
                        }
                    )*
                    Err(ParseError::NoAlternativeMatched)
                }
            }
        }
    };
}

/// fold_many0 applies a parser zero or more times and folds the results using the provided
/// function and initial accumulator value
pub fn fold_many0<I, P, O, F, Acc>(
    parser: P,
    init: Acc,
    f: F,
) -> impl Fn(I) -> ParseResult<I, Acc>
where
    I: Input,
    P: Fn(I) -> ParseResult<I, O>,
    F: Fn(Acc, O) -> Acc,
    Acc: Clone,
{
    move |mut input: I| {
        let mut acc = init.clone();

        loop {
            match parser(input) {
                Ok((rest, result)) => {
                    acc = f(acc, result);
                    input = rest;
                }
                Err(_) => break,
            }
        }

        Ok((input, acc))
    }
}

/// peek applies a parser but does not consume any input
pub fn peek<I, P, O>(parser: P) -> impl Fn(I) -> ParseResult<I, O>
where
    I: Input,
    P: Fn(I) -> ParseResult<I, O>,
{
    move |input: I| {
        let (_, result) = parser(input)?;
        Ok((input, result))
    }
}

/// fold_many1 is like fold_many0, but requires at least one successful parse
pub fn fold_many1<I, P, O, F, Acc>(
    parser: P,
    init: Acc,
    f: F,
) -> impl Fn(I) -> ParseResult<I, Acc>
where
    I: Input,
    P: Fn(I) -> ParseResult<I, O>,
    F: Fn(Acc, O) -> Acc,
    Acc: Clone,
{
    move |mut input: I| {
        let mut acc = init.clone();

        match parser(input) {
            Ok((rest, result)) => {
                acc = f(acc, result);
                input = rest;
            }
            Err(_) => return Err(ParseError::ExpectedAtLeastOne),
        }

        loop {
            match parser(input) {
                Ok((rest, result)) => {
                    acc = f(acc, result);
                    input = rest;
                }
                Err(_) => break,
            }
        }

        Ok((input, acc))
    }
}

/// take consumes a specific number of bytes from the input
pub fn take<I: Input>(count: usize) -> impl Fn(I) -> ParseResult<I, I> {
    move |input: I| {
        if input.len() >= count {
            let (matched, rest) = input.split_at(count);
            Ok((rest, matched))
        } else {
            Err(ParseError::NotEnoughInputToTake)
        }
    }
}

#[cfg(test)]
mod tests;
