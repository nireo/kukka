use kukka::*;
use std::{collections::HashMap, error::Error, fs, iter::Map};

#[derive(Clone, Debug, PartialEq)]
enum Node<'a> {
    Null,
    Boolean(bool),
    Number(i64),
    String(&'a str),

    // since we are using the hashmap the actual order of the objects is not preserved.
    Object(HashMap<&'a str, Node<'a>>),
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

fn parse_string_inner(data: &str) -> ParseResult<&str> {
    delimited(char('"'), take_while(|c| c != '"'), char('"')).parse(data)
}

fn parse_string(data: &str) -> ParseResult<Node> {
    map(parse_string_inner, |s| Node::String(s)).parse(data)
}

fn parse_object(json: &str) -> ParseResult<Node> {
    map(
        delimited(
            char('{'),
            separated(
                separated_pair(
                    delimited(multispace0(), parse_string_inner, multispace0()),
                    char(':'),
                    delimited(multispace0(), parse_json, multispace0()),
                ),
                char(','),
            ),
            char('}'),
        ),
        |v| Node::Object(v.into_iter().collect()),
    )
    .parse(json)
}

fn parse_array(json: &str) -> ParseResult<Node> {
    map(
        delimited(
            char('['),
            separated(
                parse_json,
                delimited(multispace0(), char(','), multispace0()),
            ),
            char(']'),
        ),
        |val| Node::Array(val),
    )
    .parse(json)
}

fn parse_number(data: &str) -> ParseResult<Node> {
    map(integer(), |n| Node::Number(n)).parse(data)
}

fn parse_json(data: &str) -> ParseResult<Node> {
    // TODO: yeah this sucks
    delimited(
        multispace0(),
        or(
            parse_null,
            or(
                parse_boolean,
                or(
                    parse_string,
                    or(parse_object, or(parse_array, parse_number)),
                ),
            ),
        ),
        multispace0(),
    )
    .parse(data)
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() <= 1 {
        println!("provide the file path to the csv file");
        return Ok(());
    }

    let path = &args[1];
    let content = fs::read_to_string(path)?;

    // test how long it takes to parse the file
    let start = std::time::Instant::now();

    parse_json(&content)?;
    let duration = start.elapsed();
    println!("Time elapsed in parsing is: {:?}", duration);

    Ok(())
}
