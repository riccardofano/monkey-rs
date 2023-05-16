use std::io::{self, BufRead, Write};

use crate::{lexer::Lexer, parser::Parser};

const PROMPT: &str = ">> ";

pub fn start(mut input: impl BufRead) -> io::Result<()> {
    let mut buf = String::new();

    loop {
        print!("{PROMPT}");
        io::stdout().flush()?;

        let bytes_read = input.read_line(&mut buf)?;
        if bytes_read == 0 {
            return Ok(());
        };

        let mut parser = Parser::new(Lexer::new(&buf));
        let program = parser.parse_program();

        let errors = parser.errors();
        if !errors.is_empty() {
            print_parser_errors(errors);
        }
        println!("{program}");

        buf.clear();
    }
}

fn print_parser_errors(errors: &[String]) {
    errors.iter().for_each(|e| println!("\t{e}"))
}
