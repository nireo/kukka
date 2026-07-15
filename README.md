# kukka: a small parser combinator library

Kukka is a small parser combinator library for Rust. It is inspired by `nom`, but tries to be very simple. I think the library code is good if one wants to learn how parser combinators work. 

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

The same combinators are available as extension methods for fluent composition:

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
