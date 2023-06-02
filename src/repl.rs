use std::io::{self, BufRead, Write};

use crate::{evaluator::object::Environment, evaluator::Eval, lexer::Lexer, parser::Parser};

const PROMPT: &str = ">> ";

pub fn start(mut input: impl BufRead) -> io::Result<()> {
    let mut buf = String::new();
    let env = Environment::new().into_env();

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
            continue;
        }

        let evaluated = program.eval(env.clone());
        if let Some(value) = evaluated {
            println!("{}", value.inspect());
        }

        buf.clear();
    }
}

fn print_parser_errors(errors: &[String]) {
    errors.iter().for_each(|e| println!("\t{e}"))
}
