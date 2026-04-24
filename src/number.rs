use crate::{Input, ParseError, ParseResult};

/// double parses a signed floating point number from the input
pub fn double<I: Input>(input: I) -> ParseResult<I, f64> {
    let slice = input.as_slice();
    let bytes = slice.as_ref();
    if bytes.is_empty() {
        return Err(ParseError::ExpectedAtLeastOneDigit);
    }

    let mut result = 0f64;
    let mut pos = 0;
    let mut negative = false;
    let mut saw_digit = false;

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
                saw_digit = true;
                result = result * 10.0 + (bytes[pos] - b'0') as f64;
                pos += 1;
            }
            b'.' => {
                pos += 1;
                let mut frac = 0.1;
                while pos < bytes.len() {
                    match bytes[pos] {
                        b'0'..=b'9' => {
                            saw_digit = true;
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

    if !saw_digit {
        return Err(if negative {
            ParseError::ExpectedDigitsAfterSign
        } else {
            ParseError::ExpectedAtLeastOneDigit
        });
    }

    let final_result = if negative { -result } else { result };
    let (_, rest) = input.split_at(pos);
    Ok((rest, final_result))
}

/// integer parses a signed integer from the input
pub fn integer<I: Input>(input: I) -> ParseResult<I, i64> {
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
