use crate::{AsciiDigit, Input, ParseError, ParseResult, Parser};

/// multispace0 matches zero or more whitespace characters (space, tab, newline, carriage return)
#[inline(always)]
pub fn multispace0<I: Input>(input: I) -> ParseResult<I, I> {
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

/// multispace1 is like multispace0, but requires at least one whitespace character
#[inline(always)]
pub fn multispace1<I: Input>(input: I) -> ParseResult<I, I> {
    let (rest, matched) = multispace0(input)?;
    if matched.len() == 0 {
        Err(ParseError::ExpectedWhitespace)
    } else {
        Ok((rest, matched))
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

/// take_while1 is like take_while, but requires at least one matching item.
pub fn take_while1<I, F>(p: F) -> impl Fn(I) -> ParseResult<I, I>
where
    I: Input,
    F: Fn(I::Item) -> bool,
{
    move |input: I| {
        let (rest, matched) = take_while(&p).parse(input)?;
        if matched.len() == 0 {
            Err(ParseError::ExpectedAtLeastOne)
        } else {
            Ok((rest, matched))
        }
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

/// take_until_any2 consumes input until either target item is found. The target item is not consumed.
pub fn take_until_any2<I>(target1: I::Item, target2: I::Item) -> impl Fn(I) -> ParseResult<I, I>
where
    I: Input,
    I::Item: PartialEq,
{
    move |input: I| {
        let end_pos = input
            .find_item2(target1, target2)
            .unwrap_or_else(|| input.len());

        let (matched, rest) = input.split_at(end_pos);
        Ok((rest, matched))
    }
}

/// take_until_any3 consumes input until any target item is found. The target item is not consumed.
pub fn take_until_any3<I>(
    target1: I::Item,
    target2: I::Item,
    target3: I::Item,
) -> impl Fn(I) -> ParseResult<I, I>
where
    I: Input,
    I::Item: PartialEq,
{
    move |input: I| {
        let end_pos = input
            .find_item3(target1, target2, target3)
            .unwrap_or_else(|| input.len());

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

pub fn digit01<I>(input: I) -> ParseResult<I, I>
where
    I: Input,
    I::Item: AsciiDigit,
{
    take_while(|c: I::Item| c.is_ascii_digit()).parse(input)
}

/// take consumes a specific number of bytes from the input.
///
/// For `&str`, `count` must fall on a valid UTF-8 character boundary.
pub fn take<I: Input>(count: usize) -> impl Fn(I) -> ParseResult<I, I> {
    move |input: I| {
        if input.len() < count {
            Err(ParseError::NotEnoughInputToTake)
        } else if let Some((matched, rest)) = input.split_at_checked(count) {
            Ok((rest, matched))
        } else {
            Err(ParseError::InvalidTakeBoundary)
        }
    }
}
