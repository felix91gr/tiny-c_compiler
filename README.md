# Tiny-C Compiler
A compiler of the Tiny C language, and of future expansions to it

## How to run it

### Tools

You need to have `rustc`, the Rust compiler, and `cargo`, its package manager. For that, go to the [Rustup](https://rustup.rs/) website and follow the instructions.

Rustup will help you install both `rustc`, `cargo`, keeping them up-to-date and using different versions, if you would need to.

Both for ease of use, and because I might end up using the nightly toolchain (there are certain functions that are on nightly only but are useful!), I'd 101% recommend using `rustup` :)

### Running the project

First, build the project.

```bash
cargo build
```

Then, run it feeding it some code:

```bash
cargo run -- -v parse code_samples/do_while_example.tc
cargo run -- -v add-symbols code_samples/function_call.tc
cargo run -- -v semantic-analysis code_samples/math_functions.tc
```

You can also check the included usage help:

```bash
cargo run -- --help
```

### Running the tests

To run the unit tests:

```bash
cargo test
```

## Syntax Highlighting

You can have syntax highlighting for Tiny-C on your editor of choice (if, by choice, you mean VSCode or Sublime Text!). 

It's pretty nice. Instructions are available [here](https://github.com/felix91gr/tiny-c-syntax_highlighting).
