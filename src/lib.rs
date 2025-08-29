/// input just contains a reference to a string. this is done to prevent copying.
pub type Input<'a> = &'a str;

/// a parser returns a new reference to the string that the next parsers should parse from the
/// result also contains some data given by a parser if it was successful. Other than that, we
/// return a basic static string error.
///
/// TODO: implement proper errors
pub type ParseResult<'a, T> = Result<(Input<'a>, T), &'static str>;

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

pub fn multispace0() -> impl Fn(&str) -> ParseResult<&str> {
    take_while(|c| c.is_whitespace())
}

pub fn multispace1() -> impl Fn(&str) -> ParseResult<&str> {
    move |input: &str| {
        let (rest, matched) = multispace0()(input)?;
        if matched.is_empty() {
            Err("expected at least one whitespace character")
        } else {
            Ok((rest, matched))
        }
    }
}

pub fn opt<'a, P, O>(p: P) -> impl Parser<'a, Option<O>>
where
    P: Parser<'a, O>,
{
    move |input: Input<'a>| match p.parse(input) {
        Ok((rest, res)) => Ok((rest, Some(res))),
        Err(_) => Ok((input, None)),
    }
}

/// Value returns a given value given to the function if the parser is successful, think of it as a
/// more ergonomic 'map'
///
/// TODO: prevent cloning here.
pub fn value<'a, P, V, O>(p: P, val: V) -> impl Parser<'a, V>
where
    P: Parser<'a, O>,
    V: Clone,
{
    move |input: Input<'a>| match p.parse(input) {
        Ok((rest, _)) => Ok((rest, val.clone())),
        Err(_) => Err("didnt match value parser"),
    }
}

/// Tries to apply the parser `p`. If it fails, it returns the provided default value without
/// consuming any input. This will clone the default value. TODO: avoid cloning if possible.
pub fn or_default<'a, P, O>(p: P, default: O) -> impl Parser<'a, O>
where
    P: Parser<'a, O>,
    O: Clone,
{
    move |input: Input<'a>| match p.parse(input) {
        Ok((rest, res)) => Ok((rest, res)),
        Err(_) => Ok((input, default.clone())),
    }
}

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

pub fn take_while<F>(p: F) -> impl Fn(&str) -> ParseResult<&str>
where
    F: Fn(char) -> bool,
{
    move |input: &str| {
        let end_pos = input
            .char_indices()
            .find(|(_, c)| !p(*c))
            .map(|(pos, _)| pos)
            .unwrap_or(input.len());
        Ok((&input[end_pos..], &input[..end_pos]))
    }
}

pub fn take_until(target: char) -> impl Fn(&str) -> ParseResult<&str> {
    move |input: &str| {
        let end_pos = input
            .char_indices()
            .find(|(_, c)| *c == target)
            .map(|(pos, _)| pos)
            .unwrap_or(input.len());
        Ok((&input[end_pos..], &input[..end_pos]))
    }
}

pub fn comma() -> impl Fn(&str) -> ParseResult<char> {
    move |input: &str| {
        if input.as_bytes().first() == Some(&b',') {
            Ok((&input[1..], ','))
        } else {
            Err("expected comma")
        }
    }
}

pub fn char(expected: char) -> impl Fn(&str) -> ParseResult<char> {
    move |input: &str| {
        let mut chars = input.char_indices();
        match chars.next() {
            Some((_, c)) if c == expected => {
                let next_pos = chars.next().map(|(pos, _)| pos).unwrap_or(input.len());
                Ok((&input[next_pos..], c))
            }
            _ => Err("char mismatch"),
        }
    }
}

pub fn string(expected: &'static str) -> impl Fn(&str) -> ParseResult<&str> {
    move |input: &str| {
        if input.starts_with(expected) {
            Ok((&input[expected.len()..], &input[..expected.len()]))
        } else {
            Err("string mismatch")
        }
    }
}

pub fn seq<P1, P2, O1, O2>(p1: P1, p2: P2) -> impl Fn(&str) -> ParseResult<(O1, O2)>
where
    P1: Fn(&str) -> ParseResult<O1>,
    P2: Fn(&str) -> ParseResult<O2>,
{
    move |input: &str| {
        let (rest, r1) = p1(input)?;
        let (rest, r2) = p2(rest)?;
        Ok((rest, (r1, r2)))
    }
}

pub fn alt<P1, P2, O>(p1: P1, p2: P2) -> impl Fn(&str) -> ParseResult<O>
where
    P1: Fn(&str) -> ParseResult<O>,
    P2: Fn(&str) -> ParseResult<O>,
{
    move |input: &str| p1(input).or_else(|_| p2(input))
}

pub fn many<'a, P, O>(parser: P) -> impl Parser<'a, Vec<O>>
where
    P: Parser<'a, O>,
{
    move |mut input: Input<'a>| {
        let mut res = Vec::new();

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

pub fn many1<'a, P, O>(parser: P) -> impl Parser<'a, Vec<O>>
where
    P: Parser<'a, O>,
{
    move |mut input: Input<'a>| {
        let mut res = Vec::new();
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

pub fn separated<'a, P1, P2, O1, O2>(p1: P1, p2: P2) -> impl Parser<'a, Vec<O1>>
where
    P1: Parser<'a, O1>,
    P2: Parser<'a, O2>,
{
    move |mut input: Input<'a>| {
        let mut res = Vec::new();

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

pub fn integer() -> impl Fn(&str) -> ParseResult<i64> {
    move |input: &str| {
        let (rest, digits) = digit01()(input)?;
        if digits.is_empty() {
            return Err("expected at least one digit");
        }

        match digits.parse::<i64>() {
            Ok(num) => Ok((rest, num)),
            Err(_) => Err("failed to parse integer"),
        }
    }
}

pub fn digit01() -> impl Fn(&str) -> ParseResult<&str> {
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

pub fn separated1<'a, P1, P2, O1, O2>(p1: P1, p2: P2) -> impl Parser<'a, Vec<O1>>
where
    P1: Parser<'a, O1>,
    P2: Parser<'a, O2>,
{
    move |mut input: Input<'a>| {
        let mut res = Vec::new();
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_char_success() {
        let parser = char('a');
        let result = parser("abc");
        assert_eq!(result, Ok(("bc", 'a')));
    }

    #[test]
    fn test_char_failure() {
        let parser = char('a');
        let result = parser("bc");
        assert_eq!(result, Err("char mismatch"));
    }

    #[test]
    fn test_char_empty_input() {
        let parser = char('a');
        let result = parser("");
        assert_eq!(result, Err("char mismatch"));
    }

    #[test]
    fn test_char_single_char() {
        let parser = char('x');
        let result = parser("x");
        assert_eq!(result, Ok(("", 'x')));
    }

    #[test]
    fn test_char_unicode() {
        let parser = char('ñ');
        let result = parser("ñoño");
        assert_eq!(result, Ok(("oño", 'ñ')));
    }

    #[test]
    fn test_string_success() {
        let parser = string("hello");
        let result = parser("hello world");
        assert_eq!(result, Ok((" world", "hello")));
    }

    #[test]
    fn test_string_failure() {
        let parser = string("hello");
        let result = parser("hi world");
        assert_eq!(result, Err("string mismatch"));
    }

    #[test]
    fn test_string_exact_match() {
        let parser = string("test");
        let result = parser("test");
        assert_eq!(result, Ok(("", "test")));
    }

    #[test]
    fn test_string_partial_match() {
        let parser = string("hello");
        let result = parser("hell");
        assert_eq!(result, Err("string mismatch"));
    }

    #[test]
    fn test_string_empty_string() {
        let parser = string("");
        let result = parser("anything");
        assert_eq!(result, Ok(("anything", "")));
    }

    #[test]
    fn test_seq_success() {
        let p1 = char('a');
        let p2 = char('b');
        let parser = seq(p1, p2);
        let result = parser("abc");
        assert_eq!(result, Ok(("c", ('a', 'b'))));
    }

    #[test]
    fn test_seq_first_fails() {
        let p1 = char('a');
        let p2 = char('b');
        let parser = seq(p1, p2);
        let result = parser("xbc");
        assert_eq!(result, Err("char mismatch"));
    }

    #[test]
    fn test_seq_second_fails() {
        let p1 = char('a');
        let p2 = char('b');
        let parser = seq(p1, p2);
        let result = parser("axc");
        assert_eq!(result, Err("char mismatch"));
    }

    #[test]
    fn test_alt_first_succeeds() {
        let p1 = char('a');
        let p2 = char('b');
        let parser = alt(p1, p2);
        let result = parser("abc");
        assert_eq!(result, Ok(("bc", 'a')));
    }

    #[test]
    fn test_alt_second_succeeds() {
        let p1 = char('a');
        let p2 = char('b');
        let parser = alt(p1, p2);
        let result = parser("bac");
        assert_eq!(result, Ok(("ac", 'b')));
    }

    #[test]
    fn test_alt_both_fail() {
        let p1 = char('a');
        let p2 = char('b');
        let parser = alt(p1, p2);
        let result = parser("cde");
        assert_eq!(result, Err("char mismatch"));
    }

    #[test]
    fn test_many_empty_input() {
        let char_a = char('a');
        let parser = many(char_a);
        let result = parser.parse("");
        assert_eq!(result, Ok(("", vec![])));
    }

    #[test]
    fn test_many_no_matches() {
        let char_a = char('a');
        let parser = many(char_a);
        let result = parser.parse("bcde");
        assert_eq!(result, Ok(("bcde", vec![])));
    }

    #[test]
    fn test_many_single_match() {
        let char_a = char('a');
        let parser = many(char_a);
        let result = parser.parse("abc");
        assert_eq!(result, Ok(("bc", vec!['a'])));
    }

    #[test]
    fn test_many_multiple_matches() {
        let char_a = char('a');
        let parser = many(char_a);
        let result = parser.parse("aaabcd");
        assert_eq!(result, Ok(("bcd", vec!['a', 'a', 'a'])));
    }

    #[test]
    fn test_many_consumes_all() {
        let char_a = char('a');
        let parser = many(char_a);
        let result = parser.parse("aaaa");
        assert_eq!(result, Ok(("", vec!['a', 'a', 'a', 'a'])));
    }

    #[test]
    fn test_many_with_string() {
        let hello_parser = string("hello");
        let parser = many(hello_parser);
        let result = parser.parse("hellohellohello world");
        assert_eq!(result, Ok((" world", vec!["hello", "hello", "hello"])));
    }

    #[test]
    fn test_separated_empty() {
        let digit = char('1');
        let comma = char(',');
        let parser = separated(digit, comma);

        let result = parser.parse("abc");
        assert_eq!(result, Ok(("abc", vec![])));
    }

    #[test]
    fn test_separated_single_element() {
        let digit = char('1');
        let comma = char(',');
        let parser = separated(digit, comma);

        let result = parser.parse("1abc");
        assert_eq!(result, Ok(("abc", vec!['1'])));
    }

    #[test]
    fn test_separated_multiple_elements() {
        let digit = char('1');
        let comma = char(',');
        let parser = separated(digit, comma);

        let result = parser.parse("1,1,1abc");
        assert_eq!(result, Ok(("abc", vec!['1', '1', '1'])));
    }

    #[test]
    fn test_separated_trailing_separator_error() {
        let digit = char('1');
        let comma = char(',');
        let parser = separated(digit, comma);

        let result = parser.parse("1,1,");
        assert_eq!(result, Err("expected element after separator"));
    }

    #[test]
    fn test_separated1_requires_one_element() {
        let digit = char('1');
        let comma = char(',');
        let parser = separated1(digit, comma);

        let result = parser.parse("abc");
        assert_eq!(result, Err("char mismatch"));
    }

    #[test]
    fn test_separated1_success() {
        let digit = char('1');
        let comma = char(',');
        let parser = separated1(digit, comma);

        let result = parser.parse("1,1,1abc");
        assert_eq!(result, Ok(("abc", vec!['1', '1', '1'])));
    }
}
