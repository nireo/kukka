# kukka: a small parser combinator library

Kukka is a small parser combinator library for Rust, inspired by nom. It provides a set of combinators to build complex parsers from simple ones. It is efficient, as it uses zero-copy parsing and utilizes Rust's powerful type system to prevent trait object overhead.

## Features

- Zero-copy parsing
- Composable parsers
- No trait object overhead
- Simple and easy to use

## TODO

- There are some clones in the library which most likely aren't necessary, still need to check those.

## Example

```rust
// This example is still very simplistic and doesn't validate for example that the lines
// are the correct length.
use kukka::*;
use std::{error::Error, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() <= 1 {
        println!("provide the file path to the csv file");
        return Ok(());
    }

    let path = &args[1];
    let content = fs::read_to_string(path)?;

    let field_parser = take_while(|c| c != ';' && c != '\n' && c != '\r');
    let line_parser = separated1(field_parser, char(';'));
    let newline_parser = alt(char('\n'), char('\r'));
    let csv_parser = separated1(line_parser, newline_parser);

    let (_, rows) = csv_parser.parse(&content)?;
    for row in rows {
        for field in row {
            print!("{} ", field);
        }
        println!("");
    }
    Ok(())
}
```

Other examples can be found in the `/examples` folder.
