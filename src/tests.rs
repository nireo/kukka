use crate::*;

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

    #[test]
    fn test_delimited_success() {
        let parser = delimited(char('('), char('a'), char(')'));
        let result = parser.parse("(a)");
        assert_eq!(result, Ok(("", 'a')));
    }

    #[test]
    fn test_integer_success() {
        let parser = integer();
        let result = parser.parse("123abc");
        assert_eq!(result, Ok(("abc", 123)));
    }

    #[test]
    fn test_separated_pair_success() {
        let parser = separated_pair(char('a'), char(','), char('b'));
        let result = parser.parse("a,bcd");
        assert_eq!(result, Ok(("cd", ('a', 'b'))));
    }

    #[test]
    fn test_multispace0() {
        let parser = multispace0();
        let result = parser.parse("   abc");
        assert_eq!(result, Ok(("abc", "   ")));
    }

    #[test]
    fn test_multispace0_succeeds_without_space() {
        let parser = multispace0();
        let result = parser.parse("abc");
        assert_eq!(result, Ok(("abc", "")));
    }

    #[test]
    fn test_multispace1() {
        let parser = multispace1();
        let result = parser.parse("   abc");
        assert_eq!(result, Ok(("abc", "   ")));
    }

    #[test]
    fn test_multispace1_fails_without_space() {
        let parser = multispace1();
        let result = parser.parse("abc");
        assert!(result.is_err());
    }

    #[test]
    fn test_map() {
        let parser = map(integer(), |num| num * 2);
        let result = parser.parse("21abc");
        assert_eq!(result, Ok(("abc", 42)));
    }
}
