use kukka::*;
use rustc_hash::FxHashMap;
use std::rc::Rc;
use std::{error::Error, fs};

#[derive(Clone, Debug, PartialEq)]
enum Node<'a> {
    Null,
    Boolean(bool),
    Number(i64),
    String(&'a str),

    // since we are using the hashmap the actual order of the objects is not preserved.
    Object(Rc<FxHashMap<&'a str, Node<'a>>>),
    Array(Rc<Vec<Node<'a>>>),
}

fn parse_boolean<'a>(data: &'a str) -> ParseResult<'a, Node<'a>> {
    or(
        value(string("true"), || Node::Boolean(true)),
        value(string("false"), || Node::Boolean(false)),
    )
    .parse(data)
}

fn parse_null<'a>(data: &'a str) -> ParseResult<'a, Node<'a>> {
    value(string("null"), || Node::Null).parse(data)
}

fn parse_string_inner<'a>(data: &'a str) -> ParseResult<'a, &'a str> {
    delimited(char('"'), take_while(|c| c != '"'), char('"')).parse(data)
}

fn parse_string<'a>(data: &'a str) -> ParseResult<'a, Node<'a>> {
    map(parse_string_inner, |s| Node::String(s)).parse(data)
}

fn parse_object<'a>(json: &'a str) -> ParseResult<'a, Node<'a>> {
    map(
        delimited(
            char('{'),
            separated_into_map(
                separated_pair(
                    delimited(multispace0(), parse_string_inner, multispace0()),
                    char(':'),
                    delimited(multispace0(), parse_json, multispace0()),
                ),
                char(','),
                8,
            ),
            char('}'),
        ),
        |v| Node::Object(Rc::new(v)),
    )
    .parse(json)
}

fn parse_array<'a>(json: &'a str) -> ParseResult<'a, Node<'a>> {
    map(
        delimited(
            char('['),
            separated(
                parse_json,
                delimited(multispace0(), char(','), multispace0()),
            ),
            char(']'),
        ),
        |val| Node::Array(Rc::new(val)),
    )
    .parse(json)
}

fn parse_number<'a>(data: &'a str) -> ParseResult<'a, Node<'a>> {
    map(integer(), |n| Node::Number(n)).parse(data)
}

fn parse_json<'a>(data: &'a str) -> ParseResult<'a, Node<'a>> {
    delimited(
        multispace0(),
        alt!(
            parse_string,
            parse_number,
            parse_array,
            parse_object,
            parse_null,
            parse_boolean,
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

    let start = std::time::Instant::now();

    parse_json(&content)?;
    let duration = start.elapsed();
    println!("Time elapsed in parsing is: {:?}", duration);

    Ok(())
}
