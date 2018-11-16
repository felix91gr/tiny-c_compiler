extern crate pest;
#[macro_use]
extern crate pest_derive;

mod parser;
use parser::print_sema_error;
use parser::print_parse_error;
use parser::display_ast;
use parser::parse_tc_file;

//////////////////////////////////////
//        The Program Itself        //
//////////////////////////////////////

#[macro_use]
extern crate clap;
use clap::App;

fn main() {

	use std::fs;

	let yaml = load_yaml!("cli.yml");
  let matches = App::from_yaml(yaml).get_matches();	

  println!("Running the Tiny-C Compiler...");

  match matches.occurrences_of("verbose") {
    0 => println!("Verbose mode is off"),
    1 => println!("Verbose mode is kind on. However, it's not implemented yet!"),
    _ => println!("With infinite verbosity."),
	}

  if let Some(mode) = matches.value_of("mode") {
    match mode {
      "parse" => {
        println!("Running in Parser mode");

        if let Some(input_file) = matches.value_of("INPUT") {
          println!("Reading from input file: {}", input_file);

          let unparsed_file = fs::read_to_string(input_file).expect("cannot read file");

          let parse_result = parse_tc_file(&unparsed_file);

          match parse_result {
            Ok(ast) => {
              println!("Naked (syntactic) AST:");
              println!("{}", display_ast(&ast, "  ", true));
            },
            Err(e) => print_parse_error(e),
          }
        }
      },
      "add-symbols" => {
        println!("Running Parser mode + Symbol propagation");

        if let Some(input_file) = matches.value_of("INPUT") {
          println!("Reading from input file: {}", input_file);

          let unparsed_file = fs::read_to_string(input_file).expect("cannot read file");

          let parse_result = parse_tc_file(&unparsed_file);

          match parse_result {
            Ok(mut ast) => {
              match ast.enrich_interior_scope_with_symbols() {
                Ok(_) => {
                  println!("Enriched AST:");
                  println!("{}", display_ast(&ast, "  ", false));
                }
                Err(e) => {
                  print_sema_error(e);
                }
              }
            },
            Err(e) => print_parse_error(e),
          }
        }
      },
      _ => {
        println!("(Compiler says:) I need to be updated, because there is a compilation mode that is not being handled!");
        unreachable!()
      },
    }
  }
}