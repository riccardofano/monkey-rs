# Monkey Programming Language Interpreter

This repository contains an implementation of the Monkey programming language interpreter, written in Rust. The implementation is based on the book "Writing an Interpreter in Go" by Thorsten Ball, but has been reimagined and rewritten in Rust.

The Monkey programming language is a simple interpreted language designed for educational purposes. It features C-like syntax, dynamic typing, and supports basic data types such as integers, booleans, strings, arrays, and hash maps.

## Features

The Monkey interpreter implemented in this project provides the following features:

- Lexical analysis (tokenizer): The interpreter breaks down the input source code into a sequence of tokens.
- Parsing: The tokens are parsed into an abstract syntax tree (AST) representation of the source code.
- Evaluation: The AST is evaluated, producing the corresponding output based on the Monkey language semantics.
- REPL (Read-Eval-Print Loop): The interpreter provides an interactive environment where users can enter Monkey code and see the results immediately.

## Prerequisites

To build and run the Monkey interpreter, you need to have the following dependencies installed on your system:

- [Rust](https://www.rust-lang.org/tools/install)

## Getting Started

1. Clone this repository:

   ```bash
   git clone https://github.com/riccardofano/monkey-rs.git
   ```

2. Navigate to the project directory:

   ```bash
   cd monkey-rs
   ```

3. Build and run the interpreter:

   ```bash
   cargo run
   ```

   This will start the Monkey REPL, where you can enter and execute Monkey code interactively.

## Usage

Once you have the Monkey REPL running, you can start entering Monkey code. The interpreter will evaluate the code and display the result immediately. You can use the REPL for experimentation and learning the Monkey language.

Here's an example of using the Monkey interpreter:

```plaintext
Hello! This is the Monkey programming language!
Feel free to type in commands
>> let answer = 42;
>> answer * 2;
84
>> let add = fn(x, y) { return x + y; };
>> add(10, 20);
30
```

## License

This Monkey interpreter is licensed under the [MIT License](LICENSE).

## Acknowledgments

This implementation is based on the book "Writing an Interpreter in Go" by Thorsten Ball. Many thanks to Thorsten Ball for writing such an excellent resource for learning about interpreters and language design.
