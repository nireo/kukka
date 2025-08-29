use kukka::*;
use std::{collections::HashMap, error::Error, fs, iter::Map};

#[derive(Clone)]
enum Node<'a> {
    Null,
    Boolean(bool),
    Number(f64),
    String(&'a str),

    // since we are using the hashmap the actual order of the objects is not preserved.
    Object(Box<HashMap<&'a str, Node<'a>>>),
    Array(Vec<Node<'a>>),
}

fn parse_boolean(data: &str) -> ParseResult<Node> {
    or(
        value(string("true"), Node::Boolean(true)),
        value(string("false"), Node::Boolean(false)),
    )
    .parse(data)
}

fn parse_null(data: &str) -> ParseResult<Node> {
    value(string("null"), Node::Null).parse(data)
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() <= 1 {
        println!("provide the file path to the csv file");
        return Ok(());
    }

    let path = &args[1];
    let content = fs::read_to_string(path)?;
    println!("{}", content);

    Ok(())
}
