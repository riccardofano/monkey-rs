use std::io::{self, BufRead, Write};

use crate::{lexer::Lexer, token::TokenKind};

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
        let mut token = lexer.next_token();

        while token.kind != TokenKind::Eof {
            println!("{token:?}");
            token = lexer.next_token();
        }
    }
}
