use std::hash::Hash;

use memchr::memchr;
use rustc_hash::FxHashMap;

/// initial capacity for vectors to avoid reallocations
const VECTOR_INITIAL_CAPACITY: usize = 10;

/// input just contains a reference to a string. this is done to prevent copying.
pub type Input<'a> = &'a str;

/// a parser returns a new reference to the string that the next parsers should parse from the
/// result also contains some data given by a parser if it was successful. Other than that, we
/// return a basic static string error.
///
/// TODO: implement proper errors
pub type ParseResult<'a, T> = Result<(Input<'a>, T), &'static str>;

/// A Parser is a function that takes an input string and returns a ParseResult.
pub trait Parser<'a, Out> {
    fn parse(&self, input: Input<'a>) -> ParseResult<'a, Out>;
}

impl<'a, F, Out> Parser<'a, Out> for F
where
    F: Fn(Input<'a>) -> ParseResult<'a, Out>,
{
    fn parse(&self, input: Input<'a>) -> ParseResult<'a, Out> {
        self(input)
    }
}

/// multispace0 matches zero or more whitespace characters (space, tab, newline, carriage return)
#[inline(always)]
pub fn multispace0<'a>() -> impl Parser<'a, &'a str> {
    |input: Input<'a>| {
        let bytes = input.as_bytes();
        let mut end_pos = 0;

        while end_pos < bytes.len() {
            match bytes[end_pos] {
                b' ' | b'\t' | b'\n' | b'\r' => end_pos += 1,
                _ => break,
            }
        }

        Ok((&input[end_pos..], &input[..end_pos]))
    }
}

/// multispace1 is like multispace0, but requires at least one whitespace character
#[inline(always)]
pub fn multispace1<'a>() -> impl Parser<'a, &'a str> {
    |input: Input<'a>| {
        let (rest, matched) = multispace0().parse(input)?;
        if matched.is_empty() {
            Err("expected at least one whitespace character")
        } else {
            Ok((rest, matched))
        }
    }
}

/// Value returns a given value given to the function if the parser is successful, think of it as a
/// more ergonomic 'map'
#[inline(always)]
pub fn value<'a, P, F, O, V>(p: P, val_fn: F) -> impl Parser<'a, V>
where
    P: Parser<'a, O>,
    F: Fn() -> V,
{
    move |input: Input<'a>| match p.parse(input) {
        Ok((rest, _)) => Ok((rest, val_fn())),
        Err(_) => Err("didnt match value parser"),
    }
}

/// Tries to apply the parser `p`. If it fails, it returns the provided default value without
/// consuming any input. This will clone the default value. TODO: avoid cloning if possible.
pub fn or_default<'a, P, O, D>(p: P, default: D) -> impl Parser<'a, O>
where
    P: Parser<'a, O>,
    D: Fn() -> O,
{
    move |input: Input<'a>| match p.parse(input) {
        Ok((rest, res)) => Ok((rest, res)),
        Err(_) => Ok((input, default())),
    }
}

/// or tries to apply the first parser, if it fails, it tries the second parser
#[inline(always)]
pub fn or<'a, P1, P2, O>(p1: P1, p2: P2) -> impl Parser<'a, O>
where
    P1: Parser<'a, O>,
    P2: Parser<'a, O>,
{
    move |input: Input<'a>| match p1.parse(input) {
        Ok((rest, res)) => Ok((rest, res)),
        Err(_) => p2.parse(input),
    }
}

/// and applies two parsers in sequence and returns a tuple of their results
pub fn and<'a, P1, P2, O1, O2>(p1: P1, p2: P2) -> impl Parser<'a, (O1, O2)>
where
    P1: Parser<'a, O1>,
    P2: Parser<'a, O2>,
{
    move |input: Input<'a>| {
        let (rest, r1) = p1.parse(input)?;
        let (rest, r2) = p2.parse(rest)?;
        Ok((rest, (r1, r2)))
    }
}

/// take_while consumes input while the predicate is true. If the predicate is false at the start
/// of the input, an empty string is returned.
pub fn take_while<'a, F>(p: F) -> impl Parser<'a, &'a str>
where
    F: Fn(char) -> bool,
{
    move |input: Input<'a>| {
        let bytes = input.as_bytes();
        let mut end_pos = 0;

        for &byte in bytes {
            if byte > 127 || !p(byte as char) {
                break;
            }
            end_pos += 1;
        }

        if end_pos < bytes.len() && bytes[end_pos] > 127 {
            end_pos = input
                .char_indices()
                .skip_while(|(i, _)| *i < end_pos)
                .find(|(_, c)| !p(*c))
                .map(|(pos, _)| pos)
                .unwrap_or(input.len());
        }

        Ok((&input[end_pos..], &input[..end_pos]))
    }
}

/// take_until consumes input until the target character is found. The target character is not
/// consumed. If the target character is not found, the entire input is consumed.
pub fn take_until<'a>(target: char) -> impl Parser<'a, &'a str> {
    move |input: Input<'a>| match memchr(target as u8, input.as_bytes()) {
        Some(pos) => Ok((&input[pos..], &input[..pos])),
        None => Ok(("", input)),
    }
}

/// char matches a specific character at the start of the input
#[inline(always)]
pub fn char<'a>(expected: char) -> impl Parser<'a, char> {
    move |input: Input<'a>| {
        if input.as_bytes().first() == Some(&(expected as u8)) {
            return Ok((&input[1..], expected));
        } else {
            return Err("char mismatch");
        }
    }
}

/// string matches a specific string at the start of the input
#[inline(always)]
pub fn string<'a>(expected: &'static str) -> impl Parser<'a, &'a str> {
    move |input: Input<'a>| {
        if input.starts_with(expected) {
            Ok((&input[expected.len()..], &input[..expected.len()]))
        } else {
            Err("string mismatch")
        }
    }
}

/// many applies a parser zero or more times and collects the results in a vector
pub fn many<'a, P, O>(parser: P) -> impl Parser<'a, Vec<O>>
where
    P: Parser<'a, O>,
{
    move |mut input: Input<'a>| {
        let mut res = Vec::with_capacity(VECTOR_INITIAL_CAPACITY);

        loop {
            match parser.parse(input) {
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
pub fn map<'a, P, O1, O2, F>(parser: P, f: F) -> impl Parser<'a, O2>
where
    P: Parser<'a, O1>,
    F: Fn(O1) -> O2,
{
    move |input: Input<'a>| {
        let (rest, result) = parser.parse(input)?;
        Ok((rest, f(result)))
    }
}

/// many1 applies a parser one or more times and collects the results in a vector. If the parser
/// does not match at least once, an error is returned.
pub fn many1<'a, P, O>(parser: P) -> impl Parser<'a, Vec<O>>
where
    P: Parser<'a, O>,
{
    move |mut input: Input<'a>| {
        let mut res = Vec::with_capacity(VECTOR_INITIAL_CAPACITY);
        match parser.parse(input) {
            Ok((rest, result)) => {
                res.push(result);

                loop {
                    match parser.parse(rest) {
                        Ok((rest, result)) => {
                            res.push(result);
                            input = rest;
                        }
                        Err(_) => break,
                    }
                }

                Ok((input, res))
            }
            Err(_) => Err("could not parse once"),
        }
    }
}

/// separated applies two parsers in sequence, where the second parser is a separator. The results
/// of the first parser are collected into a vector. The separator parser is not included in the
/// results. If the first parser does not match at least once, an empty vector is returned
/// instead.
pub fn separated<'a, P1, P2, O1, O2>(p1: P1, p2: P2) -> impl Parser<'a, Vec<O1>>
where
    P1: Parser<'a, O1>,
    P2: Parser<'a, O2>,
{
    move |mut input: Input<'a>| {
        let mut res = Vec::with_capacity(VECTOR_INITIAL_CAPACITY);

        match p1.parse(input) {
            Ok((rest, result)) => {
                res.push(result);
                input = rest;
            }
            Err(_) => {
                return Ok((input, res));
            }
        }

        loop {
            match p2.parse(input) {
                Ok((rest, _)) => {
                    input = rest;
                    match p1.parse(input) {
                        Ok((rest, result)) => {
                            res.push(result);
                            input = rest;
                        }
                        Err(_) => {
                            return Err("expected element after separator");
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
pub fn double<'a>() -> impl Parser<'a, f64> {
    |input: Input<'a>| {
        let bytes = input.as_bytes();
        if bytes.is_empty() {
            return Err("expected at least one digit");
        }

        let mut result = 0f64;
        let mut pos = 0;
        let mut negative = false;

        if bytes[0] == b'-' {
            negative = true;
            pos = 1;
        }

        if pos >= bytes.len() {
            return Err("expected digits after sign");
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
            return Err("expected at least one digit");
        }

        let final_result = if negative { -result } else { result };
        Ok((&input[pos..], final_result))
    }
}

/// integer parses a signed integer from the input
pub fn integer<'a>() -> impl Parser<'a, i64> {
    |input: Input<'a>| {
        let bytes = input.as_bytes();
        if bytes.is_empty() {
            return Err("expected at least one digit");
        }

        let mut result = 0i64;
        let mut pos = 0;
        let mut negative = false;

        if bytes[0] == b'-' {
            negative = true;
            pos = 1;
        }

        if pos >= bytes.len() {
            return Err("expected digits after sign");
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
            return Err("expected at least one digit");
        }

        let final_result = if negative { -result } else { result };
        Ok((&input[pos..], final_result))
    }
}

pub fn digit01<'a>() -> impl Parser<'a, &'a str> {
    take_while(|c| c.is_ascii_digit())
}

/// delimited requires that all three parsers pass, but it returns the value from the second
/// function this is especially useful for example reading the content of a string which is
/// surrounded by "" and then being able to map to that value
pub fn delimited<'a, P1, P2, P3, O1, O2, O3>(
    first_delim: P1,
    inner: P2,
    second_delim: P3,
) -> impl Parser<'a, O2>
where
    P1: Parser<'a, O1>,
    P2: Parser<'a, O2>,
    P3: Parser<'a, O3>,
{
    move |input: Input<'a>| {
        let (rest, _) = first_delim.parse(input)?;
        let (rest, out) = inner.parse(rest)?;
        let (rest, _) = second_delim.parse(rest)?;
        Ok((rest, out))
    }
}

/// separated_pair applies three parsers in sequence, but only returns the results of the first
/// and third parser. This is useful for example when parsing key-value pairs where the key and
/// value are separated by a colon or equals sign.
pub fn separated_pair<'a, P1, P2, P3, O1, O2, O3>(
    p1: P1,
    p2: P2,
    p3: P3,
) -> impl Parser<'a, (O1, O3)>
where
    P1: Parser<'a, O1>,
    P2: Parser<'a, O2>,
    P3: Parser<'a, O3>,
{
    move |input: Input<'a>| {
        let (rest, out1) = p1.parse(input)?;
        let (rest, _) = p2.parse(rest)?;
        let (rest, out3) = p3.parse(rest)?;
        Ok((rest, (out1, out3)))
    }
}

/// separated_into_map is like separated, but it expects the first parser to return a tuple of key
/// and value. The results are collected into a HashMap. If the same key is found
/// multiple times, the last value is kept. The capacity_hint is used to initialize the
/// HashMap with a certain capacity to avoid reallocations.
pub fn separated_into_map<'a, P1, P2, O, K, V>(
    p1: P1,
    p2: P2,
    capacity_hint: usize,
) -> impl Parser<'a, FxHashMap<K, V>>
where
    P1: Parser<'a, (K, V)>,
    P2: Parser<'a, O>,
    K: Hash + Eq,
{
    move |mut input: Input<'a>| {
        let mut map = FxHashMap::with_capacity_and_hasher(capacity_hint, Default::default());
        match p1.parse(input) {
            Ok((rest, (key, value))) => {
                map.insert(key, value);
                input = rest;
            }
            Err(_) => return Ok((input, map)),
        }

        loop {
            match p2.parse(input) {
                Ok((rest, _)) => {
                    input = rest;
                    match p1.parse(input) {
                        Ok((rest, (key, value))) => {
                            map.insert(key, value);
                            input = rest;
                        }
                        Err(_) => return Err("expected element after separator"),
                    }
                }
                Err(_) => break,
            }
        }
        Ok((input, map))
    }
}

/// separated1 is like separated, but requires at least one element
pub fn separated1<'a, P1, P2, O1, O2>(p1: P1, p2: P2) -> impl Parser<'a, Vec<O1>>
where
    P1: Parser<'a, O1>,
    P2: Parser<'a, O2>,
{
    move |mut input: Input<'a>| {
        let mut res = Vec::with_capacity(VECTOR_INITIAL_CAPACITY);
        let (rest, result) = p1.parse(input)?;
        res.push(result);
        input = rest;

        loop {
            match p2.parse(input) {
                Ok((rest, _)) => {
                    input = rest;
                    match p1.parse(input) {
                        Ok((rest, result)) => {
                            res.push(result);
                            input = rest;
                        }
                        Err(_) => {
                            return Err("expected element after separator");
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
        |input| {  // No explicit lifetime - let compiler infer
            match ($first).parse(input) {
                Ok(result) => Ok(result),
                Err(_) => {
                    $(
                        match ($rest).parse(input) {
                            Ok(result) => return Ok(result),
                            Err(_) => {}
                        }
                    )*
                    Err("no alternative matched")
                }
            }
        }
    };
}

/// fold_many0 applies a parser zero or more times and folds the results using the provided
/// function and initial accumulator value
pub fn fold_many0<'a, P, O, F, Acc>(parser: P, init: Acc, f: F) -> impl Parser<'a, Acc>
where
    P: Parser<'a, O>,
    F: Fn(Acc, O) -> Acc,
    Acc: Clone,
{
    move |mut input: Input<'a>| {
        let mut acc = init.clone();

        loop {
            match parser.parse(input) {
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
pub fn peek<'a, P, O>(parser: P) -> impl Parser<'a, O>
where
    P: Parser<'a, O>,
{
    move |input: Input<'a>| {
        let (_, result) = parser.parse(input)?;
        Ok((input, result))
    }
}

/// fold_many1 is like fold_many0, but requires at least one successful parse
pub fn fold_many1<'a, P, O, F, Acc>(parser: P, init: Acc, f: F) -> impl Parser<'a, Acc>
where
    P: Parser<'a, O>,
    F: Fn(Acc, O) -> Acc,
    Acc: Clone,
{
    move |mut input: Input<'a>| {
        let mut acc = init.clone();

        match parser.parse(input) {
            Ok((rest, result)) => {
                acc = f(acc, result);
                input = rest;
            }
            Err(_) => return Err("could not parse once"),
        }

        loop {
            match parser.parse(input) {
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

/// take consumes a specific number of characters from the input
pub fn take<'a>(count: usize) -> impl Parser<'a, &'a str> {
    move |input: Input<'a>| {
        if input.len() >= count {
            Ok((&input[count..], &input[..count]))
        } else {
            Err("not enough input to take")
        }
    }
}

// tests in the test.rs file
#[cfg(test)]
mod tests;
