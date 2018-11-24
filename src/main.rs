extern crate pest;

#[macro_use]
extern crate pest_derive;

#[cfg(test)]
extern crate synfuzz;

#[cfg(test)]
extern crate thousands;

mod settings;
use settings::verbosity;

mod parser;
use parser::print_parse_error;
use parser::print_dupe_error;
use parser::print_sema_error;
use parser::display_ast;
use parser::parse_tc_file;

#[cfg(test)]
mod fuzzer;

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

  unsafe {
    settings::VERBOSITY = matches.occurrences_of("verbose");
  }

  vprintln!("Running the Tiny-C Compiler...");

  match verbosity() {
    0 => println!("Verbose mode: off"),
    1 => println!("Verbose mode: on"),
    _ => println!("With infinite verbosity."),
	}

  if let Some(input_file) = matches.value_of("INPUT") {

    vprintln!("Reading from input file: {}", input_file);

    let unparsed_file = fs::read_to_string(input_file).expect("cannot read file");
    
    if let Some(mode) = matches.value_of("mode") {
      match mode {

        "parse" => {
          vprintln!("Running in Parser mode");

          let parse_result = parse_tc_file(&unparsed_file);

          match parse_result {
            Ok(ast) => {
              vprintln!("Naked (syntactic) AST:");
              println!("{}", display_ast(&ast, "  ", true));
            },
            Err(e) => print_parse_error(e),
          }
        },

        "add-symbols" => {
          vprintln!("Running Parser mode + Symbol propagation");

          let parse_result = parse_tc_file(&unparsed_file);

          match parse_result {
            Ok(mut ast) => {
              match ast.enrich_interior_scope_with_symbols() {
                Ok(_) => {
                  vprintln!("File passes symbol propagation!");
                  vprintln!("Enriched AST:");
                  println!("{}", display_ast(&ast, "  ", false));
                }
                Err(e) => {
                  print_dupe_error(e);
                }
              }
            },
            Err(e) => print_parse_error(e),
          }
        },

        "semantic-analysis" => {
          vprintln!("Running Parser + Symbol propagation + Symbol reachability analysis");

          let parse_result = parse_tc_file(&unparsed_file);

          match parse_result {
            
            Ok(mut ast) => {
              match ast.enrich_interior_scope_with_symbols() {
            
                Ok(_) => {
                  match ast.check_reachability_of_used_symbols() {

                    Ok(_) => {
                      println!("File passes the semantic analysis!");
                      vprintln!("Enriched AST:");
                      println!("{}", display_ast(&ast, "  ", false));
                    }
                    Err(e) => {
                      print_sema_error(e)
                    }
                  }
                }
                Err(e) => {
                  print_dupe_error(e);
                }
              }
            },
            Err(e) => print_parse_error(e),
          }
        },
        _ => {
          println!("(Compiler says:) I need to be updated, because there is a compilation mode that is not being handled!");
          unreachable!()
        },
      }
    }
  }
}
