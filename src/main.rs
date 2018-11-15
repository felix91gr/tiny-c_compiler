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
    1 => println!("Verbose mode is kind of on"),
    2 => println!("Verbose mode is on"),
    3 | _ => println!("Don't be crazy"),
	}

	if matches.is_present("parse-only") {
		println!("Running in Parser mode");

    if let Some(input_file) = matches.value_of("INPUT") {
      println!("Reading from input file: {}", input_file);

      let unparsed_file = fs::read_to_string(input_file).expect("cannot read file");

      let parse_result = parse_tc_file(&unparsed_file);

      match parse_result {
      	Ok(ast) => println!("Bare (purely syntactic) AST: \n{:#?}", ast),
      	Err(e) => print_error(e),
      }
		}
	}
  else if matches.is_present("add-symbols") {
    println!("Running Parser mode + Symbol propagation");

    if let Some(input_file) = matches.value_of("INPUT") {
      println!("Reading from input file: {}", input_file);

      let unparsed_file = fs::read_to_string(input_file).expect("cannot read file");

      let parse_result = parse_tc_file(&unparsed_file);

      match parse_result {
        Ok(mut ast) => {
          ast.enrich_interior_scope_with_symbols();
          // println!("AST: \n{:#?}", ast)
          println!("Enriched AST: \n{:#?}", ast)
        },
        Err(e) => print_error(e),
      }
    }
  }
}