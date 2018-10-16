# Tiny-C Compiler
A compiler of the Tiny C language, and of future expansions to it

## How to run it

### Tools

You need to have `rustc`, the Rust compiler, and `cargo`, its package manager. For that, go to the [Rustup](https://rustup.rs/) website and follow the instructions.

Rustup will help you install both `rustc`, `cargo`, keeping them up-to-date and using different versions, if you would need to.

Both for ease of use, and because I might end up using the nightly toolchain (there are certain functions that are on nightly only but are useful!), I'd 101% recommend using `rustup` :)

### Running the project

On the project's root directory, run this:

`cargo run`

That's it!

### Running the tests

This is not yet done, but I intend to use testing because it makes for a working compiler. At some point in the future, run:

`cargo test`

And that should run my (not yet made) test suite.
