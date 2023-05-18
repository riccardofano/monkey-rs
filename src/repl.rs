use std::{
    cell::RefCell,
    io::{self, BufRead, Write},
    rc::Rc,
};

use crate::{evaluator::Eval, lexer::Lexer, object::Environment, parser::Parser};

const PROMPT: &str = ">> ";

pub fn start(mut input: impl BufRead) -> io::Result<()> {
    let mut buf = String::new();
    let env = Rc::new(RefCell::new(Environment::new()));

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
        println!("{}", evaluated.inspect());

        buf.clear();
    }
}

fn print_parser_errors(errors: &[String]) {
    errors.iter().for_each(|e| println!("\t{e}"))
}
