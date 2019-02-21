extern crate pest;

#[macro_use]
extern crate pest_derive;

#[cfg(test)]
extern crate synfuzz;

#[cfg(test)]
extern crate time;

#[cfg(test)]
extern crate thousands;

mod settings;
use settings::verbosity;

mod parser;
use parser::display_ast;
use parser::parse_tc_file;
use parser::print_dupe_error;
use parser::print_parse_error;
use parser::print_sema_error;

#[cfg(test)]
mod fuzzer;

use std::path::Path;

extern crate inkwell;

use inkwell::context::Context;
use inkwell::passes::PassManager;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::OptimizationLevel;

use parser::Compiler;

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
                        }
                        Err(e) => print_parse_error(e),
                    }
                }

                "add-symbols" => {
                    vprintln!("Running Parser mode + Symbol propagation");

                    let parse_result = parse_tc_file(&unparsed_file);

                    match parse_result {
                        Ok(mut ast) => match ast.enrich_interior_scope_with_symbols() {
                            Ok(_) => {
                                vprintln!("File passes symbol propagation!");
                                vprintln!("Enriched AST:");
                                println!("{}", display_ast(&ast, "  ", false));
                            }
                            Err(e) => {
                                print_dupe_error(e);
                            }
                        },
                        Err(e) => print_parse_error(e),
                    }
                }

                "semantic-analysis" => {
                    vprintln!("Running Parser + Symbol propagation + Symbol reachability analysis");

                    let parse_result = parse_tc_file(&unparsed_file);

                    match parse_result {
                        Ok(mut ast) => match ast.enrich_interior_scope_with_symbols() {
                            Ok(_) => match ast.check_reachability_of_used_symbols() {
                                Ok(_) => {
                                    println!("File passes the semantic analysis!");
                                    vprintln!("Enriched AST:");
                                    println!("{}", display_ast(&ast, "  ", false));
                                }
                                Err(e) => print_sema_error(e),
                            },
                            Err(e) => {
                                print_dupe_error(e);
                            }
                        },
                        Err(e) => print_parse_error(e),
                    }
                }

                "compile" => {
                    vprintln!("Running Parser + Symbol propagation + Symbol reachability analysis + Compilation!");

                    let parse_result = parse_tc_file(&unparsed_file);

                    match parse_result {
                        Ok(mut ast) => {
                            match ast.enrich_interior_scope_with_symbols() {
                                Ok(_) => {
                                    match ast.check_reachability_of_used_symbols() {
                                        Ok(_) => {
                                            Target::initialize_native(
                                                &InitializationConfig::default(),
                                            )
                                            .expect("Failed to initialize native target.");

                                            let context = Context::create();
                                            let module = context.create_module(input_file);
                                            let builder = context.create_builder();

                                            // Create FPM
                                            let fpm = PassManager::create_for_function(&module);

                                            if matches.is_present("optimize") {
                                                fpm.add_instruction_combining_pass();
                                                fpm.add_reassociate_pass();
                                                fpm.add_gvn_pass();
                                                fpm.add_cfg_simplification_pass();
                                                fpm.add_basic_alias_analysis_pass();
                                                fpm.add_promote_memory_to_register_pass();
                                                fpm.add_instruction_combining_pass();
                                                fpm.add_reassociate_pass();
                                            }

                                            fpm.initialize();

                                            let compile_result = Compiler::compile(
                                                &context, &builder, &fpm, &module, &ast,
                                            );

                                            match compile_result {
                                                Ok(res) => {
                                                    vprintln!("Compilation worked! Compiled into LLVM IR:");
                                                    vprintln!(
                                                        "Main function's IR: \n{}",
                                                        res.print_to_string().to_string()
                                                    );

                                                    match module.verify() {
                                                        Ok(_) => {
                                                            vprintln!(
                                                                "Module's IR: \n{}",
                                                                module
                                                                    .print_to_string()
                                                                    .to_string()
                                                            );

                                                            if let Some(output_file) =
                                                                matches.value_of("OUTPUT")
                                                            {
                                                                let triple = TargetMachine::get_default_triple().to_string();
                                                                let target =
                                                                    Target::from_triple(&triple)
                                                                        .unwrap();
                                                                let target_machine = target
                                                                    .create_target_machine(
                                                                        &triple,
                                                                        "generic",
                                                                        "",
                                                                        OptimizationLevel::Default,
                                                                        RelocMode::Default,
                                                                        CodeModel::Default,
                                                                    )
                                                                    .unwrap();

                                                                let path = Path::new(output_file);
                                                                println!(
                                                                    "Write LLVM IR to {}",
                                                                    output_file
                                                                );

                                                                let _result = target_machine
                                                                    .write_to_file(
                                                                        &module,
                                                                        FileType::Object,
                                                                        &path,
                                                                    );
                                                            }

                                                            if matches.is_present("run") {
                                                                vprintln!("Executing with JIT compilation!");

                                                                let barrier_of_execution = ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";

                                                                println!(
                                                                    "{}",
                                                                    barrier_of_execution
                                                                );

                                                                let ee = module
                                                                    .create_jit_execution_engine(
                                                                        OptimizationLevel::Default,
                                                                    )
                                                                    .unwrap();

                                                                unsafe {
                                                                    ee.run_function(&res, &[]);
                                                                }

                                                                println!(
                                                                    "{}",
                                                                    barrier_of_execution
                                                                );
                                                            }
                                                        }
                                                        Err(e) => {
                                                            println!("However, the Module is not correct!");

                                                            println!("{}", e);
                                                        }
                                                    }
                                                }
                                                Err(e) => {
                                                    println!("Error when trying to compile to LLVM! \n{:?}", e);

                                                    vprintln!(
                                                        "Here's the Module's IR: \n{}",
                                                        module.print_to_string().to_string()
                                                    );
                                                }
                                            }
                                        }
                                        Err(e) => print_sema_error(e),
                                    }
                                }
                                Err(e) => {
                                    print_dupe_error(e);
                                }
                            }
                        }
                        Err(e) => print_parse_error(e),
                    }
                }

                _ => {
                    println!("(Compiler says:) I need to be updated, because there is a compilation mode that is not being handled!");
                    unimplemented!()
                }
            }
        }
    }
}
