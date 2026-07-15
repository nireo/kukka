use crate::*;

#[cfg(test)]
mod tests {
    use super::*;

    struct MatchA;
    struct MatchB;

    impl<'a> Parser<&'a str, char> for MatchA {
        fn parse(&self, input: &'a str) -> ParseResult<&'a str, char> {
            char('a').parse(input)
        }
    }

    impl<'a> Parser<&'a str, char> for MatchB {
        fn parse(&self, input: &'a str) -> ParseResult<&'a str, char> {
            char('b').parse(input)
        }
    }

    #[test]
    fn test_combinators_accept_parser_trait_implementors() {
        let parser = many1(MatchA);
        let result = parser.parse("aaab");
        assert_eq!(result, Ok(("b", vec!['a', 'a', 'a'])));
    }

    #[test]
    fn test_alt_macro_accepts_parser_trait_implementors() {
        let parser = alt!(MatchB, MatchA);
        let result = parser.parse("abc");
        assert_eq!(result, Ok(("bc", 'a')));
    }

    #[test]
    fn test_char_success() {
        let parser = char('a');
        let result = parser.parse("abc");
        assert_eq!(result, Ok(("bc", 'a')));
    }

    #[test]
    fn test_char_failure() {
        let parser = char('a');
        let result = parser.parse("bc");
        assert_eq!(result, Err(ParseError::CharMismatch));
    }

    #[test]
    fn test_char_empty_input() {
        let parser = char('a');
        let result = parser.parse("");
        assert_eq!(result, Err(ParseError::CharMismatch));
    }

    #[test]
    fn test_char_single_char() {
        let parser = char('x');
        let result = parser.parse("x");
        assert_eq!(result, Ok(("", 'x')));
    }

    #[test]
    fn test_char_bytes() {
        let parser = char(b'a');
        let input: &[u8] = b"abc";
        let result = parser.parse(input);
        assert_eq!(result, Ok((&b"bc"[..], b'a')));
    }

    #[test]
    fn test_string_success() {
        let parser = string("hello");
        let result = parser.parse("hello world");
        assert_eq!(result, Ok((" world", "hello")));
    }

    #[test]
    fn test_string_failure() {
        let parser = string("hello");
        let result = parser.parse("hi world");
        assert_eq!(result, Err(ParseError::StringMismatch));
    }

    #[test]
    fn test_string_exact_match() {
        let parser = string("test");
        let result = parser.parse("test");
        assert_eq!(result, Ok(("", "test")));
    }

    #[test]
    fn test_string_partial_match() {
        let parser = string("hello");
        let result = parser.parse("hell");
        assert_eq!(result, Err(ParseError::StringMismatch));
    }

    #[test]
    fn test_string_empty_string() {
        let parser = string("");
        let result = parser.parse("anything");
        assert_eq!(result, Ok(("anything", "")));
    }

    #[test]
    fn test_string_bytes() {
        let expected: &[u8] = b"hi";
        let parser = string(expected);
        let input: &[u8] = b"hi!!";
        let result = parser.parse(input);
        assert_eq!(result, Ok((&b"!!"[..], &b"hi"[..])));
    }

    #[test]
    fn test_or_first_succeeds() {
        let p1 = char('a');
        let p2 = char('b');
        let parser = or(p1, p2);
        let result = parser.parse("abc");
        assert_eq!(result, Ok(("bc", 'a')));
    }

    #[test]
    fn test_or_second_succeeds() {
        let p1 = char('a');
        let p2 = char('b');
        let parser = or(p1, p2);
        let result = parser.parse("bac");
        assert_eq!(result, Ok(("ac", 'b')));
    }

    #[test]
    fn test_or_both_fail() {
        let p1 = char('a');
        let p2 = char('b');
        let parser = or(p1, p2);
        let result = parser.parse("cde");
        assert_eq!(result, Err(ParseError::CharMismatch));
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
    fn test_many_rejects_zero_progress_parser() {
        let parser = many(multispace0);
        let result = parser.parse("abc");
        assert_eq!(result, Err(ParseError::NoProgress));
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
    fn test_many1_single_match_advances_input() {
        let parser = many1(char('a'));
        let result = parser.parse("abcd");
        assert_eq!(result, Ok(("bcd", vec!['a'])));
    }

    #[test]
    fn test_many1_rejects_zero_progress_parser() {
        let parser = many1(multispace0);
        let result = parser.parse("abc");
        assert_eq!(result, Err(ParseError::NoProgress));
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
    fn test_separated_stops_on_zero_progress_separator() {
        let parser = separated(char('a'), multispace0);
        let result = parser.parse("ab");
        assert_eq!(result, Ok(("b", vec!['a'])));
    }

    #[test]
    fn test_separated_trailing_separator_error() {
        let digit = char('1');
        let comma = char(',');
        let parser = separated(digit, comma);

        let result = parser.parse("1,1,");
        assert_eq!(result, Err(ParseError::ExpectedElementAfterSeparator));
    }

    #[test]
    fn test_separated1_requires_one_element() {
        let digit = char('1');
        let comma = char(',');
        let parser = separated1(digit, comma);

        let result = parser.parse("abc");
        assert_eq!(result, Err(ParseError::CharMismatch));
    }

    #[test]
    fn test_separated1_success() {
        let digit = char('1');
        let comma = char(',');
        let parser = separated1(digit, comma);

        let result = parser.parse("1,1,1abc");
        assert_eq!(result, Ok(("abc", vec!['1', '1', '1'])));
    }

    #[test]
    fn test_separated1_fold_requires_one_element() {
        let parser = separated1_fold(integer, char(','), Vec::new, |mut values, value| {
            values.push(value);
            values
        });

        let result = parser.parse("abc");
        assert_eq!(result, Err(ParseError::ExpectedAtLeastOneDigit));
    }

    #[test]
    fn test_separated1_fold_success() {
        let parser = separated1_fold(integer, char(','), Vec::new, |mut values, value| {
            values.push(value);
            values
        });

        let result = parser.parse("1,2,3abc");
        assert_eq!(result, Ok(("abc", vec![1, 2, 3])));
    }

    #[test]
    fn test_separated1_fold_trailing_separator_error() {
        let parser = separated1_fold(integer, char(','), Vec::new, |mut values, value| {
            values.push(value);
            values
        });

        let result = parser.parse("1,2,");
        assert_eq!(result, Err(ParseError::ExpectedElementAfterSeparator));
    }

    #[test]
    fn test_delimited_success() {
        let parser = delimited(char('('), char('a'), char(')'));
        let result = parser.parse("(a)");
        assert_eq!(result, Ok(("", 'a')));
    }

    #[test]
    fn test_separated_pair_success() {
        let parser = separated_pair(char('a'), char(','), char('b'));
        let result = parser.parse("a,bcd");
        assert_eq!(result, Ok(("cd", ('a', 'b'))));
    }

    #[test]
    fn test_separated_fold_collects_into_accumulator() {
        let parser = separated_fold(
            separated_pair(take_while(|c| c != '='), char('='), integer),
            char(','),
            std::collections::HashMap::new,
            |mut map, (key, value)| {
                map.insert(key, value);
                map
            },
        );

        let result = parser.parse("a=1,b=2 end");
        let mut expected = std::collections::HashMap::new();
        expected.insert("a", 1);
        expected.insert("b", 2);

        assert_eq!(result, Ok((" end", expected)));
    }

    #[test]
    fn test_multispace0() {
        let parser = multispace0;
        let result = parser.parse("   abc");
        assert_eq!(result, Ok(("abc", "   ")));
    }

    #[test]
    fn test_multispace0_succeeds_without_space() {
        let parser = multispace0;
        let result = parser.parse("abc");
        assert_eq!(result, Ok(("abc", "")));
    }

    #[test]
    fn test_multispace1() {
        let parser = multispace1;
        let result = parser.parse("   abc");
        assert_eq!(result, Ok(("abc", "   ")));
    }

    #[test]
    fn test_multispace1_fails_without_space() {
        let parser = multispace1;
        let result = parser.parse("abc");
        assert!(result.is_err());
    }

    #[test]
    fn test_take_while1_success() {
        let parser = take_while1(|c: char| c.is_ascii_alphabetic());
        let result = parser.parse("abc123");
        assert_eq!(result, Ok(("123", "abc")));
    }

    #[test]
    fn test_take_while1_requires_one_match() {
        let parser = take_while1(|c: char| c.is_ascii_alphabetic());
        let result = parser.parse("123abc");
        assert_eq!(result, Err(ParseError::ExpectedAtLeastOne));
    }

    #[test]
    fn test_take_until_byte2_stops_at_first_target() {
        let parser = take_until_byte2(b',', b';');
        let result = parser.parse("abc,def;ghi");
        assert_eq!(result, Ok((",def;ghi", "abc")));
    }

    #[test]
    fn test_take_until_byte3_stops_at_first_target() {
        let parser = take_until_byte3(b';', b'\n', b'\r');
        let result = parser.parse("field\nnext");
        assert_eq!(result, Ok(("\nnext", "field")));
    }

    #[test]
    fn test_take_until_byte3_bytes() {
        let parser = take_until_byte3(b';', b'\n', b'\r');
        let input: &[u8] = b"field;next";
        let result = parser.parse(input);
        assert_eq!(result, Ok((&b";next"[..], &b"field"[..])));
    }

    #[test]
    fn test_take_until_byte3_rejects_invalid_utf8_boundary() {
        let parser = take_until_byte3(0xa9, b'x', b'y');
        let result = parser.parse("éa");
        assert_eq!(result, Err(ParseError::InvalidTakeBoundary));
    }

    #[test]
    fn test_map() {
        let parser = map(integer, |num| num * 2);
        let result = parser.parse("21abc");
        assert_eq!(result, Ok(("abc", 42)));
    }

    #[test]
    fn test_parser_ext_chaining() {
        let parser = integer.map(|num| num * 2).and(multispace1).many1();
        let result = parser.parse("1 2 3 end");
        assert_eq!(result, Ok(("end", vec![(2, " "), (4, " "), (6, " ")])));
    }

    #[test]
    fn test_parser_ext_delimited_by() {
        let parser = char('a').delimited_by(char('('), char(')'));
        let result = parser.parse("(a)rest");
        assert_eq!(result, Ok(("rest", 'a')));
    }

    #[test]
    fn test_double() {
        let parser = double;
        let result = parser.parse("3.14xyz");
        assert_eq!(result, Ok(("xyz", 3.14)));
    }

    #[test]
    fn test_double_no_decimal() {
        let parser = double;
        let result = parser.parse("42xyz");
        assert_eq!(result, Ok(("xyz", 42.0)));
    }

    #[test]
    fn test_double_requires_at_least_one_digit() {
        let parser = double;
        let result = parser.parse(".xyz");
        assert_eq!(result, Err(ParseError::ExpectedAtLeastOneDigit));
    }

    #[test]
    fn test_double_requires_digits_after_sign() {
        let parser = double;
        let result = parser.parse("-.xyz");
        assert_eq!(result, Err(ParseError::ExpectedDigitsAfterSign));
    }

    #[test]
    fn test_integer() {
        let parser = integer;
        let result = parser.parse("456def");
        assert_eq!(result, Ok(("def", 456)));
    }

    #[test]
    fn test_integer_negative() {
        let parser = integer;
        let result = parser.parse("-789ghi");
        assert_eq!(result, Ok(("ghi", -789)));
    }

    #[test]
    fn test_integer_no_digits() {
        let parser = integer;
        let result = parser.parse("abc");
        assert!(result.is_err());
    }

    #[test]
    fn test_digit01() {
        let parser = digit01;
        let result = parser.parse("123abc");
        assert_eq!(result, Ok(("abc", "123")));
    }

    #[test]
    fn test_alt_macro_works() {
        let parser = alt!(char('a'), char('b'), char('c'));
        let result = parser.parse("cxyz");
        assert_eq!(result, Ok(("xyz", 'c')));
    }

    #[test]
    fn test_alt_macro_all_fail() {
        let parser = alt!(char('a'), char('b'), char('c'));
        let result = parser.parse("xyz");
        assert_eq!(result, Err(ParseError::NoAlternativeMatched));
    }

    #[test]
    fn test_value() {
        let parser = value(string("true"), || true);
        let result = parser.parse("trueabc");
        assert_eq!(result, Ok(("abc", true)));
    }

    #[test]
    fn test_value_no_match() {
        let parser = value(string("true"), || true);
        let result = parser.parse("falseabc");
        assert_eq!(result, Err(ParseError::StringMismatch));
    }

    #[test]
    fn test_or_default() {
        let p1 = char('x');
        let p2 = char('y');
        let parser = or_default(or(p1, p2), || 'z');
        let result1 = parser.parse("xyz");
        let result2 = parser.parse("abc");
        assert_eq!(result1, Ok(("yz", 'x')));
        assert_eq!(result2, Ok(("abc", 'z')));
    }

    #[test]
    fn test_fold_many_returns_init_with_empty() {
        let parser = fold_many0(integer, || 0, |acc, num| acc + num);
        let res = parser.parse("");
        assert_eq!(res, Ok(("", 0)));
    }

    #[test]
    fn test_fold_many_rejects_zero_progress_parser() {
        let parser = fold_many0(multispace0, || 0usize, |acc, _| acc + 1);
        let result = parser.parse("abc");
        assert_eq!(result, Err(ParseError::NoProgress));
    }

    #[test]
    fn test_fold_many_sums_integers() {
        let parser = fold_many0(and(integer, multispace1), || 0, |acc, num| acc + num.0);
        let res = parser.parse("1 2 3 abc");
        assert_eq!(res, Ok(("abc", 6)));
    }

    #[test]
    fn test_fold_many_string_concat() {
        let parser = fold_many0(
            map(char('a'), |c: char| c.to_string()),
            String::new,
            |mut acc, s| {
                acc.push_str(&s);
                acc
            },
        );

        let result = parser.parse("aaaabbb");
        assert_eq!(result, Ok(("bbb", "aaaa".to_string())));
    }

    #[test]
    fn test_fold_many_supports_non_clone_accumulators() {
        #[derive(Debug, PartialEq, Eq)]
        struct Count(usize);

        let parser = fold_many0(
            char('a'),
            || Count(0),
            |mut acc, _| {
                acc.0 += 1;
                acc
            },
        );

        let result = parser.parse("aaabbb");
        assert_eq!(result, Ok(("bbb", Count(3))));
    }

    #[test]
    fn test_fold_many1_requires_one() {
        let parser = fold_many1(char('a'), || 0usize, |acc, _| acc + 1);
        let result = parser.parse("bbb");
        assert!(result.is_err());

        let result = parser.parse("aaabbb");
        assert_eq!(result, Ok(("bbb", 3)));
    }

    #[test]
    fn test_fold_many1_rejects_zero_progress_parser() {
        let parser = fold_many1(multispace0, || 0usize, |acc, _| acc + 1);
        let result = parser.parse("abc");
        assert_eq!(result, Err(ParseError::NoProgress));
    }

    #[test]
    fn test_take_rejects_invalid_utf8_boundary() {
        let parser = take(1);
        let result = parser.parse("éa");
        assert_eq!(result, Err(ParseError::InvalidTakeBoundary));
    }

    #[test]
    fn test_take_bytes_success() {
        let parser = take(2);
        let input: &[u8] = b"abcd";
        let result = parser.parse(input);
        assert_eq!(result, Ok((&b"cd"[..], &b"ab"[..])));
    }

    #[test]
    fn test_or_backtracks_after_partial_match() {
        let parser = or(
            value(delimited(char('a'), char('b'), char('c')), || 1),
            value(string("abx"), || 2),
        );

        assert_eq!(parser.parse("abx"), Ok(("", 2)));
    }

    #[test]
    fn test_or_returns_second_error_when_both_parsers_fail() {
        let parser = or(value(char('a'), || ()), value(string("x"), || ()));

        assert_eq!(parser.parse("z"), Err(ParseError::StringMismatch));
    }

    #[test]
    fn test_or_default_restores_input_after_partial_match() {
        let parser = or_default(
            value(delimited(char('a'), char('b'), char('c')), || 1),
            || 0,
        );

        assert_eq!(parser.parse("abx"), Ok(("abx", 0)));
    }

    #[test]
    fn test_peek_does_not_consume_input() {
        let parser = peek(char('a'));

        assert_eq!(parser.parse("abc"), Ok(("abc", 'a')));
    }

    #[test]
    fn test_peek_preserves_parser_error() {
        let parser = peek(string("expected"));

        assert_eq!(parser.parse("other"), Err(ParseError::StringMismatch));
    }

    #[test]
    fn test_many_rejects_successful_peek_without_progress() {
        let parser = many(peek(char('a')));

        assert_eq!(parser.parse("abc"), Err(ParseError::NoProgress));
    }

    #[test]
    fn test_separated1_trailing_separator_error() {
        let parser = separated1(char('1'), char(','));

        assert_eq!(
            parser.parse("1,1,"),
            Err(ParseError::ExpectedElementAfterSeparator)
        );
    }

    #[test]
    fn test_separated1_stops_on_zero_progress_separator() {
        let parser = separated1(char('a'), multispace0);

        assert_eq!(parser.parse("ab"), Ok(("b", vec!['a'])));
    }

    #[test]
    fn test_separated_fold_empty_input_uses_initializer() {
        let parser = separated_fold(integer, char(','), || 10, |sum, value| sum + value);

        assert_eq!(parser.parse("abc"), Ok(("abc", 10)));
    }

    #[test]
    fn test_separated_fold_trailing_separator_error() {
        let parser = separated_fold(integer, char(','), || 0, |sum, value| sum + value);

        assert_eq!(
            parser.parse("1,2,"),
            Err(ParseError::ExpectedElementAfterSeparator)
        );
    }

    #[test]
    fn test_separated_fold_stops_on_zero_progress_separator() {
        let parser = separated_fold(char('a'), multispace0, || 0, |count, _| count + 1);

        assert_eq!(parser.parse("ab"), Ok(("b", 1)));
    }

    #[test]
    fn test_separated_fold_creates_fresh_accumulator_for_each_parse() {
        let parser = separated_fold(integer, char(','), Vec::new, |mut values, value| {
            values.push(value);
            values
        });

        assert_eq!(parser.parse("1,2"), Ok(("", vec![1, 2])));
        assert_eq!(parser.parse("3"), Ok(("", vec![3])));
    }

    #[test]
    fn test_separated1_fold_stops_on_zero_progress_separator() {
        let parser = separated1_fold(char('a'), multispace0, || 0, |count, _| count + 1);

        assert_eq!(parser.parse("ab"), Ok(("b", 1)));
    }

    #[test]
    fn test_char_and_string_support_multibyte_utf8() {
        assert_eq!(char('é').parse("éclair"), Ok(("clair", 'é')));
        assert_eq!(string("é").parse("éclair"), Ok(("clair", "é")));
    }

    #[test]
    fn test_take_while_tracks_utf8_boundaries() {
        let parser = take_while(|c: char| c.is_alphabetic());

        assert_eq!(parser.parse("éö123"), Ok(("123", "éö")));
    }

    #[test]
    fn test_take_until_supports_non_ascii_targets() {
        let parser = take_until('ö');

        assert_eq!(parser.parse("hélloöworld"), Ok(("öworld", "héllo")));
        assert_eq!(parser.parse("öworld"), Ok(("öworld", "")));
        assert_eq!(parser.parse("hello"), Ok(("", "hello")));
    }

    #[test]
    fn test_take_handles_zero_exact_and_oversized_counts() {
        assert_eq!(take(0).parse("éx"), Ok(("éx", "")));
        assert_eq!(take(2).parse("éx"), Ok(("x", "é")));
        assert_eq!(take(4).parse("éx"), Err(ParseError::NotEnoughInputToTake));
    }

    #[test]
    fn test_take_until_byte2_boundary_cases() {
        assert_eq!(take_until_byte2(b',', b';').parse(",abc"), Ok((",abc", "")));
        assert_eq!(take_until_byte2(b',', b';').parse("abc"), Ok(("", "abc")));
        assert_eq!(
            take_until_byte2(b',', b';').parse("abc;def,ghi"),
            Ok((";def,ghi", "abc"))
        );
        assert_eq!(
            take_until_byte2(0xa9, b'x').parse("éa"),
            Err(ParseError::InvalidTakeBoundary)
        );
    }

    #[test]
    fn test_take_until_byte3_boundary_cases() {
        assert_eq!(
            take_until_byte3(b',', b';', b'|').parse("abc|def;ghi,jkl"),
            Ok(("|def;ghi,jkl", "abc"))
        );
        assert_eq!(
            take_until_byte3(b',', b';', b'|').parse("plain"),
            Ok(("", "plain"))
        );
        assert_eq!(
            take_until_byte3(b',', b';', b'|').parse(";first"),
            Ok((";first", ""))
        );
    }

    #[test]
    fn test_byte_search_allows_arbitrary_binary_targets() {
        let input: &[u8] = &[0xff, 0xa9, 0x00];

        assert_eq!(
            take_until_byte2(0xa9, b'x').parse(input),
            Ok((&input[1..], &input[..1]))
        );
        assert_eq!(
            take_until_byte3(0x00, b'x', b'y').parse(input),
            Ok((&input[2..], &input[..2]))
        );
    }

    #[test]
    fn test_generic_parsers_support_byte_slices() {
        let integer_input: &[u8] = b"-42\xff";
        let double_input: &[u8] = b"12.5\xff";
        let whitespace_input: &[u8] = b" \t\xff";
        let digits_input: &[u8] = b"123\xff";
        let letters_input: &[u8] = b"abc\xff";

        assert_eq!(integer(integer_input), Ok((&integer_input[3..], -42)));
        assert_eq!(double(double_input), Ok((&double_input[4..], 12.5)));
        assert_eq!(
            multispace1(whitespace_input),
            Ok((&whitespace_input[2..], &whitespace_input[..2]))
        );
        assert_eq!(
            digit01(digits_input),
            Ok((&digits_input[3..], &digits_input[..3]))
        );
        assert_eq!(
            take_while(|byte: u8| byte.is_ascii_alphabetic()).parse(letters_input),
            Ok((&letters_input[3..], &letters_input[..3]))
        );
        assert_eq!(
            take_until(0xff).parse(letters_input),
            Ok((&letters_input[3..], &letters_input[..3]))
        );
    }

    #[test]
    fn test_double_prefix_grammar() {
        assert_eq!(double("-3.25x"), Ok(("x", -3.25)));
        assert_eq!(double("1.x"), Ok(("x", 1.0)));
        assert_eq!(double("1.2.3"), Ok((".3", 1.2)));
        assert_eq!(double("1e3"), Ok(("e3", 1.0)));
    }

    #[test]
    fn test_double_reports_exact_errors_for_invalid_prefixes() {
        assert_eq!(double(""), Err(ParseError::ExpectedAtLeastOneDigit));
        assert_eq!(double("-"), Err(ParseError::ExpectedDigitsAfterSign));
        assert_eq!(double("."), Err(ParseError::ExpectedAtLeastOneDigit));
        assert_eq!(double("-.x"), Err(ParseError::ExpectedDigitsAfterSign));
    }

    #[test]
    fn test_integer_maximum_and_error_cases() {
        assert_eq!(integer("9223372036854775807rest"), Ok(("rest", i64::MAX)));
        assert_eq!(integer(""), Err(ParseError::ExpectedAtLeastOneDigit));
        assert_eq!(integer("-"), Err(ParseError::ExpectedDigitsAfterSign));
        assert_eq!(integer("x"), Err(ParseError::ExpectedAtLeastOneDigit));
        assert_eq!(integer("-x"), Err(ParseError::ExpectedAtLeastOneDigit));
    }

    #[test]
    fn test_parser_extension_methods() {
        assert_eq!(char('a').value(|| 1).parse("ab"), Ok(("b", 1)));
        assert_eq!(char('a').or(char('b')).parse("bc"), Ok(("c", 'b')));
        assert_eq!(char('a').or_default(|| 'z').parse("bc"), Ok(("bc", 'z')));
        assert_eq!(char('a').many().parse("aab"), Ok(("b", vec!['a', 'a'])));
        assert_eq!(
            char('a').separated(char(',')).parse("a,ab"),
            Ok(("b", vec!['a', 'a']))
        );
        assert_eq!(
            char('a').separated1(char(',')).parse("a,ab"),
            Ok(("b", vec!['a', 'a']))
        );
        assert_eq!(char('a').peek().parse("ab"), Ok(("ab", 'a')));
        assert_eq!(
            char('a')
                .fold_many0(|| 0, |count, _| count + 1)
                .parse("aab"),
            Ok(("b", 2))
        );
        assert_eq!(
            char('a')
                .fold_many1(|| 0, |count, _| count + 1)
                .parse("aab"),
            Ok(("b", 2))
        );
    }

    #[test]
    fn test_parse_error_display_messages() {
        let cases = [
            (ParseError::CharMismatch, "char mismatch"),
            (ParseError::StringMismatch, "string mismatch"),
            (
                ParseError::ExpectedWhitespace,
                "expected at least one whitespace character",
            ),
            (ParseError::ValueParserMismatch, "didnt match value parser"),
            (ParseError::ExpectedAtLeastOne, "could not parse once"),
            (
                ParseError::ExpectedElementAfterSeparator,
                "expected element after separator",
            ),
            (
                ParseError::ExpectedAtLeastOneDigit,
                "expected at least one digit",
            ),
            (
                ParseError::ExpectedDigitsAfterSign,
                "expected digits after sign",
            ),
            (ParseError::NoAlternativeMatched, "no alternative matched"),
            (
                ParseError::NoProgress,
                "parser succeeded without consuming input",
            ),
            (ParseError::NotEnoughInputToTake, "not enough input to take"),
            (
                ParseError::InvalidTakeBoundary,
                "take count did not fall on a valid input boundary",
            ),
        ];

        for (error, expected) in cases {
            assert_eq!(error.to_string(), expected);
        }
    }
}
