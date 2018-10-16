
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(dead_code)]

#[derive(Debug)]
enum Term {
	Id(char),
	Int(u32),
	Expr(Box<Expression>)
}

#[derive(Debug)]
enum Sum {
	Tm(Term),
	Summation(Term, Box<Sum>),
	Substraction(Term, Box<Sum>)
}

#[derive(Debug)]
enum Expression {
	Assignment(char, Box<Expression>),
	Comparison(Sum, Sum),
	Value(Sum),
	Parenthesis(Box<Expression>),
}

#[derive(Debug)]
enum TCStatement {
	Empty,
	Expr(Expression),
	Scope(Vec<TCStatement>),
	DoWhile(Box<TCStatement>, Expression),
	While(Expression, Box<TCStatement>),
	IfElse(Expression, Box<TCStatement>, Box<TCStatement>),
	If(Expression, Box<TCStatement>),
}

extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "tiny-c.pest"]
struct TinyCParser;

use pest::error::Error;
use pest::iterators::Pair;	


fn parse_tc_file(file: &str) -> Result<TCStatement, Error<Rule>> {
    
		// println!("Feeding into Parser...");

    let tiny_c = TinyCParser::parse(Rule::tiny_c, file)?.next().unwrap();

		// println!("Parser accepted it... Time to generate AST...");
	
		fn parse_term(pair: Pair<Rule>) -> Term {
			match pair.as_rule() {
				Rule::id => Term::Id(pair.as_str().chars().next().unwrap()),

				Rule::int => Term::Int(pair.as_str().parse().unwrap()),

				Rule::paren_expr => {

					let expr = parse_expression(pair.into_inner().next().unwrap());

					Term::Expr(Box::new(expr))
				},

				Rule::term => parse_term(pair.into_inner().next().unwrap()),

				_ => {
					println!("Pair: {:?}", pair);
					unreachable!()
				},
			}
		}

		fn parse_sum(pair: Pair<Rule>) -> Sum {
			match pair.as_rule() {
				Rule::term => {
					let term = parse_term(pair.into_inner().next().unwrap());

					Sum::Tm(term)
				},

				Rule::summation => {

					let mut inner_pieces = pair.into_inner();

					let term = parse_term(inner_pieces.next().unwrap());

					let sum = parse_sum(inner_pieces.next().unwrap());

					Sum::Summation(term, Box::new(sum))
				},

				Rule::substraction => {

					let mut inner_pieces = pair.into_inner();

					let term = parse_term(inner_pieces.next().unwrap());

					let sum = parse_sum(inner_pieces.next().unwrap());

					Sum::Substraction(term, Box::new(sum))
				}

				Rule::sum => parse_sum(pair.into_inner().next().unwrap()),

				_ => {
					unreachable!()
				}
			}
		}

    fn parse_expression(pair: Pair<Rule>) -> Expression {
    	match pair.as_rule() {
    		
    		Rule::assignment => {
	    			let mut inner_pair = pair.into_inner();

	    			let id : char = inner_pair
	    				.next()
	    				.unwrap()
	    				.as_str()
	    				.chars()
	    				.next()
	    				.unwrap();

	    			let expression = parse_expression(inner_pair.next().unwrap());

	    			Expression::Assignment(id, Box::new(expression))    			
    			},
    		
    		Rule::paren_expr => {

    			let inner_expression = parse_expression(pair.into_inner().next().unwrap());

    			Expression::Parenthesis(Box::new(inner_expression))
    		},

    		Rule::comparison => {
    			
					let mut inner_pieces = pair.into_inner();

					let first_sum = parse_sum(inner_pieces.next().unwrap());

					let second_sum = parse_sum(inner_pieces.next().unwrap());

					Expression::Comparison(first_sum, second_sum)
    		},

    		Rule::sum => Expression::Value(parse_sum(pair.into_inner().next().unwrap())),

    		Rule::expr => parse_expression(pair.into_inner().next().unwrap()),

    		_ => {
    			println!("{:?}", pair);
    			println!("{:?}", pair.as_rule());
    			unreachable!()
    		},
    	}
    }

    fn parse_statement(pair: Pair<Rule>) -> TCStatement {
    	match pair.as_rule() {

    		Rule::statement => parse_statement(pair.into_inner().next().unwrap()),

    		Rule::semicolon_statement => TCStatement::Empty,
      	
      	Rule::expr_statement => {

      		let expr = parse_expression(pair.into_inner().next().unwrap());

      		TCStatement::Expr(expr)
      	},

      	Rule::scoped_statement => {

      		let inner_statements = pair.into_inner()
      			.map(parse_statement).collect();

      		TCStatement::Scope(inner_statements)
      	},
				
				Rule::do_while => {

					let mut inner_pieces = pair.into_inner();

					let stmt = parse_statement(inner_pieces.next().unwrap());

					let expr = parse_expression(inner_pieces.next().unwrap());

					TCStatement::DoWhile(Box::new(stmt), expr)
				},

				Rule::_while => {

					let mut inner_pieces = pair.into_inner();

					let expr = parse_expression(inner_pieces.next().unwrap());

					let stmt = parse_statement(inner_pieces.next().unwrap());

					TCStatement::While(expr, Box::new(stmt))
				},

				Rule::if_else => {

					let mut inner_pieces = pair.into_inner();

					let expr = parse_expression(inner_pieces.next().unwrap());

					let first_stmt = parse_statement(inner_pieces.next().unwrap());
					
					let second_stmt = parse_statement(inner_pieces.next().unwrap());

					TCStatement::IfElse(expr, Box::new(first_stmt), Box::new(second_stmt))
				},

				Rule::_if => {

					let mut inner_pieces = pair.into_inner();

					let expr = parse_expression(inner_pieces.next().unwrap());

					let stmt = parse_statement(inner_pieces.next().unwrap());

					TCStatement::If(expr, Box::new(stmt))
				}

				_ => unreachable!(),
    	}
    }

    Ok(parse_statement(tiny_c))
}


fn main() {
	// let unparsed_file = fs::read_to_string("program.tc").expect("cannot read file");
	// let unparsed_file = "{a;}";

	let unparsed_files = vec![
		"{i;}", 
		"{((((((i))))));}", 
		"a<1;", 
		"a<(1<2);", 
		"while (i<100) i=i+i;", 
		"{ i=1; while (i<100) i=i+i; }", 
		"{ i=125; j=100; while (i-j) if (i<j) j=j-i; else i=i-j; }",
		"{ i=1; do i=i+10; while (i<50); }",
		"{ i=1; while ((i=i+10)<50) ; }",
		"{ i=7; if (i<5) x=1; if (i<10) y=2; }",
		];

	for unparsed_file in unparsed_files {
		println!("File: {:?}", unparsed_file);

	  let tiny_c = parse_tc_file(&unparsed_file).expect("unsuccessful parse");

	  println!("{:?}", tiny_c);
	}

	// println!("{}", serialize_jsonvalue(&json));
}


