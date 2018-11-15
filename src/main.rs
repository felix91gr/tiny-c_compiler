extern crate pest;
#[macro_use]
extern crate pest_derive;

mod parser;
use parser::print_error;
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
            Ok(ast) => println!("Naked (syntactic) AST: \n{:#?}", ast),
            Err(e) => print_error(e),
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
              ast.enrich_interior_scope_with_symbols();
              println!("Enriched AST: \n{:#?}", ast)
            },
            Err(e) => print_error(e),
          }
        }
      },
      _ => unreachable!(),
    }
  }
}