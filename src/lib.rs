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

fn char_parser(expected: char) -> impl Fn(&str) -> ParseResult<char> {
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

fn string_parser(expected: &'static str) -> impl Fn(&str) -> ParseResult<&str> {
    move |input: &str| {
        if input.starts_with(expected) {
            Ok((&input[expected.len()..], &input[..expected.len()]))
        } else {
            Err("string mismatch")
        }
    }
}

fn seq<P1, P2, O1, O2>(p1: P1, p2: P2) -> impl Fn(&str) -> ParseResult<(O1, O2)>
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
