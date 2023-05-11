use std::io::{self, BufRead, Write};

use crate::lexer::Lexer;

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

        let mut lexer = Lexer::new(&buf);

        while let Some(token) = lexer.next_token() {
            println!("{token:?}");
        }
        buf.clear()
    }
}
