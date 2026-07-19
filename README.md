# kukka: a small parser combinator library

> kukka stands for flower in Finnish

Kukka is a small parser combinator library for Rust. It is inspired by `nom`, but tries to be very simple. I think the library code is good if one wants to learn how parser combinators work. Excluding tests the library is just below 1k lines, so going through the code will not take too much time. However, currently the inlining of certain parsers is not properly tested/benchmarked rather the logic is just keeping small and common parsers inlined.

The under 1k lines should be taken with a grain of salt as this code can ultimately be made a lot shorter as this contains some methods that are generally convenience methods. They are just to show that ultimately the primitives that a parser combinator library can be very flexible but not that ergonomic. For example:
- `digit0` could be written as `take_while(|c| c.is_ascii_digit())`
- `value(parser, make_value)` could be written as `map(parser, |_| make_value())`
- `many` and `many1` are `fold_many0` and `fold_many1` configured to collect into a `Vec`
- `separated` and `separated1` are `separated_fold` and `separated1_fold` configured to collect into a `Vec`

## Examples

Every parser takes an input and returns the unconsumed input together with its output. Parsers can
be composed using free functions:

```rust
use kukka::*;

fn main() -> Result<(), ParseError> {
    let identifier = take_while1(|c: char| c.is_ascii_alphabetic() || c == '_');
    let equals = delimited(multispace0, char('='), multispace0);
    let assignment = separated_pair(identifier, equals, integer);

    let (rest, (name, value)) = assignment.parse("retries = -3;")?;
    assert_eq!(rest, ";");
    assert_eq!((name, value), ("retries", -3));
    Ok(())
}
```

The main idea behind parser combinators is that we can combine different parses together:

```rust
use kukka::*;

let number = integer.delimited_by(multispace0, multispace0);
let numbers = number.separated1(char(','));

assert_eq!(
    numbers.parse("1, -2, 3 rest"),
    Ok(("rest", vec![1, -2, 3])),
);
```

Alternatives, mapping, and repeated parsing can be combined without introducing a custom parser
type:

```rust
use kukka::*;

let boolean = alt!(
    value(string("true"), || true),
    value(string("false"), || false),
);
let booleans = boolean.separated1(char(','));

assert_eq!(
    booleans.parse("true,false,true!"),
    Ok(("!", vec![true, false, true])),
);
```

Most primitives work with both `&str` and `&[u8]` input:

```rust
use kukka::*;

assert_eq!(char('k').parse("kukka"), Ok(("ukka", 'k')));
assert_eq!(
    string(&b"GET"[..]).parse(&b"GET /index.html"[..]),
    Ok((&b" /index.html"[..], &b"GET"[..])),
);
```

For larger examples, see the complete executable parsers:

- [CSV parser](examples/csv.rs)
- [JSON parser](examples/json.rs)

Run one with:

```sh
cargo run --example json -- path/to/file.json
```
