# Tiny-C Compiler
A compiler of the Tiny C language, and of future expansions to it

## How to run it

### Tools

You need to have `rustc`, the Rust compiler, and `cargo`, its package manager. For that, go to the [Rustup](https://rustup.rs/) website and follow the instructions.

Rustup will help you install both `rustc`, `cargo`, keeping them up-to-date and using different versions, if you would need to.

Both for ease of use, and because I might end up using the nightly toolchain (there are certain functions that are on nightly only but are useful!), I'd 101% recommend using `rustup` :)

### Running the project

Now that I've implemented the unit tests, the `main` function does nothing. In the future, it will input `stdin` to the parser. For the moment, please run the tests instead.

### Running the tests

To run the unit tests:

`cargo test`

