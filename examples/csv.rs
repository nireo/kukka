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
