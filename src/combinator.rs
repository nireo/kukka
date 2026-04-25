use crate::{Input, ParseError, ParseResult, Parser};

/// initial capacity for vectors to avoid reallocations
const VECTOR_INITIAL_CAPACITY: usize = 10;

#[inline(always)]
fn parser_made_progress<I: Input>(input: I, rest: I) -> bool {
    rest.len() < input.len()
}

/// Value returns a given value given to the function if the parser is successful, think of it as a
/// more ergonomic 'map'
#[inline(always)]
pub fn value<I, P, F, O, V>(p: P, val_fn: F) -> impl Fn(I) -> ParseResult<I, V>
where
    I: Input,
    P: Parser<I, O>,
    F: Fn() -> V,
{
    move |input: I| {
        let (rest, _) = p.parse(input)?;
        Ok((rest, val_fn()))
    }
}

/// Tries to apply the parser `p`. If it fails, it returns the provided default value without
/// consuming any input. This will clone the default value.
pub fn or_default<I, P, O, D>(p: P, default: D) -> impl Fn(I) -> ParseResult<I, O>
where
    I: Input,
    P: Parser<I, O>,
    D: Fn() -> O,
{
    move |input: I| match p.parse(input) {
        Ok((rest, res)) => Ok((rest, res)),
        Err(_) => Ok((input, default())),
    }
}

/// or tries to apply the first parser, if it fails, it tries the second parser
#[inline(always)]
pub fn or<I, P1, P2, O>(p1: P1, p2: P2) -> impl Fn(I) -> ParseResult<I, O>
where
    I: Input,
    P1: Parser<I, O>,
    P2: Parser<I, O>,
{
    move |input: I| match p1.parse(input) {
        Ok((rest, res)) => Ok((rest, res)),
        Err(_) => p2.parse(input),
    }
}

/// and applies two parsers in sequence and returns a tuple of their results
pub fn and<I, P1, P2, O1, O2>(p1: P1, p2: P2) -> impl Fn(I) -> ParseResult<I, (O1, O2)>
where
    I: Input,
    P1: Parser<I, O1>,
    P2: Parser<I, O2>,
{
    move |input: I| {
        let (rest, r1) = p1.parse(input)?;
        let (rest, r2) = p2.parse(rest)?;
        Ok((rest, (r1, r2)))
    }
}

/// many applies a parser zero or more times and collects the results in a vector
pub fn many<I, P, O>(parser: P) -> impl Fn(I) -> ParseResult<I, Vec<O>>
where
    I: Input,
    P: Parser<I, O>,
{
    move |mut input: I| {
        let mut res = Vec::with_capacity(VECTOR_INITIAL_CAPACITY);

        loop {
            match parser.parse(input) {
                Ok((rest, result)) => {
                    if !parser_made_progress(input, rest) {
                        return Err(ParseError::NoProgress);
                    }

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
    P: Parser<I, O1>,
    F: Fn(O1) -> O2,
{
    move |input: I| {
        let (rest, result) = parser.parse(input)?;
        Ok((rest, f(result)))
    }
}

/// many1 applies a parser one or more times and collects the results in a vector. If the parser
/// does not match at least once, an error is returned.
pub fn many1<I, P, O>(parser: P) -> impl Fn(I) -> ParseResult<I, Vec<O>>
where
    I: Input,
    P: Parser<I, O>,
{
    move |mut input: I| {
        let mut res = Vec::with_capacity(VECTOR_INITIAL_CAPACITY);
        match parser.parse(input) {
            Ok((rest, result)) => {
                if !parser_made_progress(input, rest) {
                    return Err(ParseError::NoProgress);
                }

                res.push(result);
                input = rest;

                loop {
                    match parser.parse(input) {
                        Ok((rest, result)) => {
                            if !parser_made_progress(input, rest) {
                                return Err(ParseError::NoProgress);
                            }

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
    P1: Parser<I, O1>,
    P2: Parser<I, O2>,
{
    move |mut input: I| {
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
                    if !parser_made_progress(input, rest) {
                        break;
                    }

                    input = rest;
                    match p1.parse(input) {
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
    P1: Parser<I, O1>,
    P2: Parser<I, O2>,
    P3: Parser<I, O3>,
{
    move |input: I| {
        let (rest, _) = first_delim.parse(input)?;
        let (rest, out) = inner.parse(rest)?;
        let (rest, _) = second_delim.parse(rest)?;
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
    P1: Parser<I, O1>,
    P2: Parser<I, O2>,
    P3: Parser<I, O3>,
{
    move |input: I| {
        let (rest, out1) = p1.parse(input)?;
        let (rest, _) = p2.parse(rest)?;
        let (rest, out3) = p3.parse(rest)?;
        Ok((rest, (out1, out3)))
    }
}

/// separated_fold is like separated, but folds each parsed element into an accumulator.
pub fn separated_fold<I, P1, P2, O1, O2, Init, Fold, Acc>(
    p1: P1,
    p2: P2,
    init: Init,
    fold: Fold,
) -> impl Fn(I) -> ParseResult<I, Acc>
where
    I: Input,
    P1: Parser<I, O1>,
    P2: Parser<I, O2>,
    Init: Fn() -> Acc,
    Fold: Fn(Acc, O1) -> Acc,
{
    move |mut input: I| {
        let mut acc = init();

        match p1.parse(input) {
            Ok((rest, result)) => {
                acc = fold(acc, result);
                input = rest;
            }
            Err(_) => return Ok((input, acc)),
        }

        loop {
            match p2.parse(input) {
                Ok((rest, _)) => {
                    if !parser_made_progress(input, rest) {
                        break;
                    }

                    input = rest;
                    match p1.parse(input) {
                        Ok((rest, result)) => {
                            acc = fold(acc, result);
                            input = rest;
                        }
                        Err(_) => return Err(ParseError::ExpectedElementAfterSeparator),
                    }
                }
                Err(_) => break,
            }
        }
        Ok((input, acc))
    }
}

/// separated1_fold is like separated_fold, but requires at least one element.
pub fn separated1_fold<I, P1, P2, O1, O2, Init, Fold, Acc>(
    p1: P1,
    p2: P2,
    init: Init,
    fold: Fold,
) -> impl Fn(I) -> ParseResult<I, Acc>
where
    I: Input,
    P1: Parser<I, O1>,
    P2: Parser<I, O2>,
    Init: Fn() -> Acc,
    Fold: Fn(Acc, O1) -> Acc,
{
    move |mut input: I| {
        let mut acc = init();
        let (rest, result) = p1.parse(input)?;
        acc = fold(acc, result);
        input = rest;

        loop {
            match p2.parse(input) {
                Ok((rest, _)) => {
                    if !parser_made_progress(input, rest) {
                        break;
                    }

                    input = rest;
                    match p1.parse(input) {
                        Ok((rest, result)) => {
                            acc = fold(acc, result);
                            input = rest;
                        }
                        Err(_) => return Err(ParseError::ExpectedElementAfterSeparator),
                    }
                }
                Err(_) => break,
            }
        }
        Ok((input, acc))
    }
}

/// separated1 is like separated, but requires at least one element
pub fn separated1<I, P1, P2, O1, O2>(p1: P1, p2: P2) -> impl Fn(I) -> ParseResult<I, Vec<O1>>
where
    I: Input,
    P1: Parser<I, O1>,
    P2: Parser<I, O2>,
{
    move |mut input: I| {
        let mut res = Vec::with_capacity(VECTOR_INITIAL_CAPACITY);
        let (rest, result) = p1.parse(input)?;
        res.push(result);
        input = rest;

        loop {
            match p2.parse(input) {
                Ok((rest, _)) => {
                    if !parser_made_progress(input, rest) {
                        break;
                    }

                    input = rest;
                    match p1.parse(input) {
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
            match $crate::Parser::parse(&$first, input) {
                Ok(result) => Ok(result),
                Err(_) => {
                    $(
                        match $crate::Parser::parse(&$rest, input) {
                            Ok(result) => return Ok(result),
                            Err(_) => {}
                        }
                    )*
                    Err($crate::ParseError::NoAlternativeMatched)
                }
            }
        }
    };
}

/// fold_many0 applies a parser zero or more times and folds the results using the provided
/// function and accumulator initializer.
pub fn fold_many0<I, P, O, Init, F, Acc>(
    parser: P,
    init: Init,
    f: F,
) -> impl Fn(I) -> ParseResult<I, Acc>
where
    I: Input,
    P: Parser<I, O>,
    Init: Fn() -> Acc,
    F: Fn(Acc, O) -> Acc,
{
    move |mut input: I| {
        let mut acc = init();

        loop {
            match parser.parse(input) {
                Ok((rest, result)) => {
                    if !parser_made_progress(input, rest) {
                        return Err(ParseError::NoProgress);
                    }

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
    P: Parser<I, O>,
{
    move |input: I| {
        let (_, result) = parser.parse(input)?;
        Ok((input, result))
    }
}

/// fold_many1 is like fold_many0, but requires at least one successful parse
pub fn fold_many1<I, P, O, Init, F, Acc>(
    parser: P,
    init: Init,
    f: F,
) -> impl Fn(I) -> ParseResult<I, Acc>
where
    I: Input,
    P: Parser<I, O>,
    Init: Fn() -> Acc,
    F: Fn(Acc, O) -> Acc,
{
    move |mut input: I| {
        let mut acc = init();

        match parser.parse(input) {
            Ok((rest, result)) => {
                if !parser_made_progress(input, rest) {
                    return Err(ParseError::NoProgress);
                }

                acc = f(acc, result);
                input = rest;
            }
            Err(_) => return Err(ParseError::ExpectedAtLeastOne),
        }

        loop {
            match parser.parse(input) {
                Ok((rest, result)) => {
                    if !parser_made_progress(input, rest) {
                        return Err(ParseError::NoProgress);
                    }

                    acc = f(acc, result);
                    input = rest;
                }
                Err(_) => break,
            }
        }

        Ok((input, acc))
    }
}
