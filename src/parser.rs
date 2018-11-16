//////////////////////////////////////
//          Usage of Pest           //
//////////////////////////////////////

use std::collections::HashSet;

use pest::Parser;

#[derive(Parser)]
#[grammar = "tiny-c.pest"]
struct TinyCParser;

use pest::error::Error;
use pest::error::ErrorVariant;
use pest::iterators::Pair;  
use pest::Span;

//////////////////////////////////////
//          AST Definitions         //
//////////////////////////////////////

// type Span = (usize, usize);

///////////////
//  Id Node  //
///////////////

#[derive(Debug)]
struct Identifier<'i> {
  span : Span<'i>,
  inner : char,
}

impl<'i> Identifier<'i> {
  fn new(span: Span, inner: char) -> Identifier {
    Identifier {
      span,
      inner
    }
  }
}

///////////////
// Term Node //
///////////////

#[derive(Debug)]
struct Term<'i> {
  span : Span<'i>,
  inner : InnerTerm<'i>,
}

#[derive(Debug)]
enum InnerTerm<'i> {
  Id(Identifier<'i>),
  Int(u32),
  Expr(Box<Expression<'i>>)
}

impl<'i> Term<'i> {
  fn new(span: Span<'i>, inner: InnerTerm<'i>) -> Term<'i> {
    Term {
      span,
      inner
    }
  }
}

//////////////
// Sum Node //
//////////////

#[derive(Debug)]
struct Sum<'i> {
  span : Span<'i>,
  inner : InnerSum<'i>,
}

#[derive(Debug)]
enum InnerSum<'i> {
  Tm(Term<'i>),
  Summation(Term<'i>, Box<Sum<'i>>),
  Substraction(Term<'i>, Box<Sum<'i>>)
}

impl<'i> Sum<'i> {
  fn new(span: Span<'i>, inner: InnerSum<'i>) -> Sum<'i> {
    Sum {
      span,
      inner
    }
  }
}

///////////////
// Expr Node //
///////////////

#[derive(Debug)]
enum ComparisonKind {
  LessThanOrEqual,
  LessThan,
  GreaterThanOrEqual,
  GreaterThan,
  Equal,
}

#[derive(Debug)]
struct Expression<'i> {
  span : Span<'i>,
  inner : InnerExpression<'i>,
}

#[derive(Debug)]
enum InnerExpression<'i> {
  Assignment(char, Box<Expression<'i>>),
  Comparison(ComparisonKind, Sum<'i>, Sum<'i>),
  Value(Sum<'i>),
  Parenthesis(Box<Expression<'i>>),
}

impl<'i> Expression<'i> {
  fn new(span: Span<'i>, inner: InnerExpression<'i>) -> Expression<'i> {
    Expression {
      span,
      inner
    }
  }
}

///////////////
// Ptbl Node //
///////////////

#[derive(Debug)]
struct Printable<'i> {
  span : Span<'i>,
  inner : InnerPrintable<'i>,
}

#[derive(Debug)]
enum InnerPrintable<'i> {
  Str(String),
  Expr(Expression<'i>),
}

impl<'i> Printable<'i> {
  fn new(span: Span<'i>, inner: InnerPrintable<'i>) -> Printable<'i> {
    Printable {
      span,
      inner
    }
  }
}

///////////////
// Stmt Node //
///////////////

#[derive(Debug)]
pub struct Statement<'i> {
  span : Span<'i>,
  visible_symbols : HashSet<String>,
  inner : InnerStatement<'i>,
}

#[derive(Debug)]
enum InnerStatement<'i> {
  Empty,
  Expr(Expression<'i>),
  Scope(Vec<Statement<'i>>),
  DoWhile(Box<Statement<'i>>, Expression<'i>),
  While(Expression<'i>, Box<Statement<'i>>),
  IfElse(Expression<'i>, Box<Statement<'i>>, Box<Statement<'i>>),
  If(Expression<'i>, Box<Statement<'i>>),
  PrintStatement(Printable<'i>),
  FnDeclarationStatement(String, Vec<Identifier<'i>>, Box<Statement<'i>>),
  FnUsageStatement(String, Vec<Expression<'i>>),
}

impl<'i> Statement<'i> {
  fn new(span: Span<'i>, inner: InnerStatement<'i>) -> Statement<'i> {

    let visible_symbols = HashSet::new();

    Statement {
      span,
      inner,
      visible_symbols,
    }
  }

  #[allow(dead_code)]
  fn get_own_symbol(&self) -> Option<String> {
    match &self.inner {
      InnerStatement::FnDeclarationStatement(s, identifiers, _) => {
        let symbol = format!("{}({})", s, identifiers.len());
        Some(symbol)
      },
      _ => None,
    }
  }

  // FIXME: recursively descend through inner scopes?
  #[allow(unreachable_code)]
  #[allow(dead_code)]
  pub fn get_used_symbols(&self) -> Vec<String> {
    unimplemented!();
    match &self.inner {
      InnerStatement::FnUsageStatement(s, exprs) => {
        let symbol = format!("{}({})", s, exprs.len());
        vec![symbol]
      },
      _ => Vec::new(),
    }
  }

  #[allow(dead_code)]
  fn add_visible_symbols(&mut self, visible_symbols: &HashSet<String>, depth: usize) -> Result<String, Error<Rule>> {
    
    for symbol in visible_symbols.clone().into_iter() {
      if let Some(my_symbol) = self.get_own_symbol() {
        if depth > 0 && my_symbol == symbol {
          let error_message = format!("Duplicated symbol: {}", symbol);

          let error = self.make_semantic_error(error_message);

          return Err(error);
        }
      }

      self.visible_symbols.insert(symbol);
    }

    let new_depth = depth + 1;

    // Recursive Step
    match self.inner {
      InnerStatement::Scope(ref mut ss) => {
        for s in ss.iter_mut() {
          s.add_visible_symbols(visible_symbols, new_depth)?;
        }
      },

      InnerStatement::DoWhile(ref mut s, _) |
      InnerStatement::While(_, ref mut s) |
      InnerStatement::If(_, ref mut s) |
      InnerStatement::FnDeclarationStatement(_, _, ref mut s) => { 
        s.add_visible_symbols(visible_symbols, new_depth)?; 
      },

      InnerStatement::IfElse(_, ref mut s1, ref mut s2) => {
        s1.add_visible_symbols(visible_symbols, new_depth)?;
        s2.add_visible_symbols(visible_symbols, new_depth)?;
      },

      // Empty, Expr, PrintStmt, FnUsageStmt
      _ => {},
    }

    Ok("Symbols added successfully".to_string())
  }

  #[allow(dead_code)]
  fn make_semantic_error(&self, message: String) -> Error<Rule> {
    let variant = ErrorVariant::CustomError { message };

    Error::new_from_span(variant, self.span.clone())
  } 

  #[allow(dead_code)]
  pub fn enrich_interior_scope_with_symbols(&mut self) -> Result<String, Error<Rule>> {
    
    ////// Step 0:
    ////// find current scope
    let mut scope : Vec<&mut Statement> = Vec::new();

    match self.inner {
      InnerStatement::Scope(ref mut ss) => {
        for s in ss.iter_mut() {
          scope.push(s);
        }
      },

      InnerStatement::DoWhile(ref mut s, _) |
      InnerStatement::If(_, ref mut s) |
      InnerStatement::FnDeclarationStatement(_, _, ref mut s) |
      InnerStatement::While(_, ref mut s) => scope.push(s),

      InnerStatement::IfElse(_, ref mut s1, ref mut s2) => {
        scope.push(s1);
        scope.push(s2);
      },

      // Empty, Expr, PrintStmt, FnUsageStmt
      _ => {},
    };

    ////// Step 1: 
    ////// Gather Symbols
    let mut visible_symbols = HashSet::new();

    for statement in scope.iter() {
      if let Some(symbol) = statement.get_own_symbol() {
        visible_symbols.insert(symbol);        
      }
    }

    ////// Step 2:
    ////// Add this scope's Symbols recursively
    for statement in scope.iter_mut() {
      statement.add_visible_symbols(&visible_symbols, 0)?;
    }

    ////// Step 3: 
    ////// Recurse over the interior of this statement 
    for statement in scope.iter_mut() {
      statement.enrich_interior_scope_with_symbols()?;
    }

    // If everything went well, we return a success message.
    Ok("Symbol enrichment successful".to_string())
  }
}

//////////////////////////////////////
//           AST Display            //
//////////////////////////////////////

use std::fmt::Write;

pub fn display_ast(ast: &Statement, separator: &str, hide_symbols: bool) -> String {
  
  let mut s = String::new();

  write!(&mut s, "{:#?}", ast);

  if hide_symbols {
    let mut t = String::new();

    for line in s.lines() {
      if !line.contains("visible_symbols") {
        t.push_str(line);
        t.push_str("\n");
      }
    }

    s = t;
  }

  s.replace("    ", separator)
}

//////////////////////////////////////
//         AST Construction         //
//////////////////////////////////////

pub fn parse_tc_file(file: &str) -> Result<Statement, Error<Rule>> {

    let tiny_c = TinyCParser::parse(Rule::tiny_c, file)?.next().unwrap();

    fn parse_identifier(pair: Pair<Rule>) -> Identifier {

      let span = pair.as_span();

      let inner = match pair.as_rule() {

        Rule::id => pair.as_str().chars().next().unwrap(),

        _ => unreachable!()
      };

      Identifier::new(span, inner)
    }

    fn parse_term(pair: Pair<Rule>) -> Term {
      
      let span = pair.as_span();

      let inner = match pair.as_rule() {
        Rule::id => InnerTerm::Id(parse_identifier(pair)),

        Rule::int => InnerTerm::Int(pair.as_str().parse().unwrap()),

        Rule::paren_expr => {

          let expr = parse_expression(pair.into_inner().next().unwrap());

          InnerTerm::Expr(Box::new(expr))
        },

        // In case I messed up the parsing, I do a new recursion step and ignore it
        Rule::term => parse_term(pair.into_inner().next().unwrap()).inner,

        _ => {
          println!("Pair: {:?}", pair);
          unreachable!()
        },
      };

      Term::new(span, inner)
    }

    fn parse_sum(pair: Pair<Rule>) -> Sum {
      
      let span = pair.as_span();

      let inner = match pair.as_rule() {
        Rule::term => {
          let term = parse_term(pair.into_inner().next().unwrap());

          InnerSum::Tm(term)
        },

        Rule::summation => {

          let mut inner_pieces = pair.into_inner();

          let term = parse_term(inner_pieces.next().unwrap());

          let sum = parse_sum(inner_pieces.next().unwrap());

          InnerSum::Summation(term, Box::new(sum))
        },

        Rule::substraction => {

          let mut inner_pieces = pair.into_inner();

          let term = parse_term(inner_pieces.next().unwrap());

          let sum = parse_sum(inner_pieces.next().unwrap());

          InnerSum::Substraction(term, Box::new(sum))
        }

        // In case I messed up the parsing, I do a new recursion step and ignore it
        Rule::sum => parse_sum(pair.into_inner().next().unwrap()).inner,

        _ => {
          unreachable!()
        }
      };

      Sum::new(span, inner)
    }

    fn parse_expression(pair: Pair<Rule>) -> Expression {
            
      let span = pair.as_span();

      let inner = match pair.as_rule() {

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

            InnerExpression::Assignment(id, Box::new(expression))          
          },
        
        Rule::paren_expr => {

          let inner_expression = parse_expression(pair.into_inner().next().unwrap());

          InnerExpression::Parenthesis(Box::new(inner_expression))
        },

        Rule::comparison => {
          
          let mut inner_pieces = pair.into_inner();

          let first_sum = parse_sum(inner_pieces.next().unwrap());

          let comparison_kind = match inner_pieces.next().unwrap().as_rule() {

            Rule::leq => ComparisonKind::LessThanOrEqual,

            Rule::lt => ComparisonKind::LessThan,
            
            Rule::geq => ComparisonKind::GreaterThanOrEqual,
            
            Rule::gt => ComparisonKind::GreaterThan,
            
            Rule::eq => ComparisonKind::Equal,

            _ => unreachable!()
          };

          let second_sum = parse_sum(inner_pieces.next().unwrap());

          InnerExpression::Comparison(comparison_kind, first_sum, second_sum)
        },

        Rule::sum => InnerExpression::Value(parse_sum(pair.into_inner().next().unwrap())),

        // In case I messed up the parsing, I do a new recursion step and ignore it
        Rule::expr => parse_expression(pair.into_inner().next().unwrap()).inner,

        _ => {
          println!("{:?}", pair);
          println!("{:?}", pair.as_rule());
          unreachable!()
        },
      };

      Expression::new(span, inner)
    }

    fn parse_printable(pair: Pair<Rule>) -> Printable {
                  
      let span = pair.as_span();

      let inner = match pair.as_rule() {

        Rule::string => {
          let string = pair.as_str();

          InnerPrintable::Str(string.to_string())
        },

        Rule::expr => {
          let expr = parse_expression(pair.into_inner().next().unwrap());

          InnerPrintable::Expr(expr)
        },

        Rule::printable => parse_printable(pair.into_inner().next().unwrap()).inner,

        _ => unreachable!(),
      };

      Printable::new(span, inner)
    }

    fn parse_statement(pair: Pair<Rule>) -> Statement {
            
      let span = pair.as_span();

      let inner = match pair.as_rule() {

        // In case I messed up the parsing, I do a new recursion step and ignore it
        Rule::statement => parse_statement(pair.into_inner().next().unwrap()).inner,

        Rule::semicolon_statement => InnerStatement::Empty,
        
        Rule::expr_statement => {

          let expr = parse_expression(pair.into_inner().next().unwrap());

          InnerStatement::Expr(expr)
        },

        Rule::scoped_statement => {

          let inner_statements = pair.into_inner()
            .map(parse_statement).collect();

          InnerStatement::Scope(inner_statements)
        },
        
        Rule::do_while => {

          let mut inner_pieces = pair.into_inner();

          let stmt = parse_statement(inner_pieces.next().unwrap());

          let expr = parse_expression(inner_pieces.next().unwrap());

          InnerStatement::DoWhile(Box::new(stmt), expr)
        },

        Rule::_while => {

          let mut inner_pieces = pair.into_inner();

          let expr = parse_expression(inner_pieces.next().unwrap());

          let stmt = parse_statement(inner_pieces.next().unwrap());

          InnerStatement::While(expr, Box::new(stmt))
        },

        Rule::if_else => {

          let mut inner_pieces = pair.into_inner();

          let expr = parse_expression(inner_pieces.next().unwrap());

          let first_stmt = parse_statement(inner_pieces.next().unwrap());
          
          let second_stmt = parse_statement(inner_pieces.next().unwrap());

          InnerStatement::IfElse(expr, Box::new(first_stmt), Box::new(second_stmt))
        },

        Rule::_if => {

          let mut inner_pieces = pair.into_inner();

          let expr = parse_expression(inner_pieces.next().unwrap());

          let stmt = parse_statement(inner_pieces.next().unwrap());

          InnerStatement::If(expr, Box::new(stmt))
        },

        Rule::print_statement => {

          let mut inner_pieces = pair.into_inner();

          let ptbl = parse_printable(inner_pieces.next().unwrap());

          InnerStatement::PrintStatement(ptbl)
        },

        Rule::function_declaration_statement => {

          let mut inner_pieces = pair.into_inner();

          let name = inner_pieces.next().unwrap().as_str().to_string();

          let param_list : Vec<Identifier> = inner_pieces.next().unwrap().into_inner().map(parse_identifier).collect();

          let body = Box::new(parse_statement(inner_pieces.next().unwrap()));

          InnerStatement::FnDeclarationStatement(name, param_list, body)
        },

        Rule::function_usage_statement => {
          
          let mut inner_pieces = pair.into_inner();

          let name = inner_pieces.next().unwrap().as_str().to_string();

          let param_list : Vec<Expression> = inner_pieces.next().unwrap().into_inner().map(parse_expression).collect();

          InnerStatement::FnUsageStatement(name, param_list)
        },

        _ => unreachable!(),
      };

      Statement::new(span, inner)
    }

    Ok(parse_statement(tiny_c))
}

pub fn print_parse_error(pest_error: Error<Rule>) {
  println!("Couldn't parse file.");
  println!("Error seems to be here: \n{}", pest_error);
}

pub fn print_sema_error(pest_error: Error<Rule>) {
  println!("File fails to pass semantic analysis");
  println!("Error seems to be here: \n{}", pest_error);
}

//////////////////////////////////////
//            Unit Tests            //
//////////////////////////////////////

#[cfg(test)]
mod accepts {

  use super::super::*;

  #[test]
  fn empty_statement() {
    parse_tc_file(";").unwrap();
  }

  mod basic {
    use super::super::super::*;

    #[test]
    fn _if() {
      parse_tc_file("if (i) ;").unwrap();      
    }

    #[test]
    fn if_else() {
      parse_tc_file("if (i) ; else ;").unwrap();      
    }

    #[test]
    fn do_while() {
      parse_tc_file("do ; while (i) ;").unwrap();      
    }

    #[test]
    fn _while() {
      parse_tc_file("while (i) ;").unwrap();      
    }

    #[test]
    fn scope() {
      parse_tc_file("{;}").unwrap();      
    }


    #[test]
    fn comparisons() {
      let file = r#"
        {
          a < b;
          a > b;

          a == b;

          a >= b;
          a <= b;
        }
      "#;

      parse_tc_file(file).unwrap();      
    }

    #[test]
    fn line_comments() {
      let file = r#"
        {
          // This is a comment above a statement
          a = 1;
        }
      "#;

      parse_tc_file(file).unwrap();      
    }

    #[test]
    fn multiline_comments() {
      let file = r#"
        {
          /*
            Multiline 
            comments 
            ftw!
          */
          a = 2;
        }
      "#;

      parse_tc_file(file).unwrap();      
    }

    #[test]
    fn expression_statement() {
      parse_tc_file("1 ;").unwrap();      
    }

    #[test]
    fn printing_of_strings() {
      let file = r#"
        {
          print("Hello");
        }
      "#;

      parse_tc_file(file).unwrap();      
    }

    #[test]
    fn printing_of_expressions() {
      parse_tc_file("print (a + 2) ;").unwrap();      
    }

    #[test]
    fn function_declarations() {
      let file = r#"
        {
          fn double(a) {
            a = a + a;
          }
        }
      "#;

      parse_tc_file(file).unwrap();
    }

    #[test]
    fn function_usage() {
      let file = r#"
        {
          call double(a);
        }
      "#;

      parse_tc_file(file).unwrap();
    }
  }

  mod scopes_like {
    use super::super::super::*;
    
    #[test]
    fn while_inc_i_by_i() {
      let file = r#"
        {
          i = 1; 
          while (i < 100) 
            i = i + i;
        }
      "#;

      parse_tc_file(file).unwrap();
    }

    #[test]
    fn while_and_if() {
      let file = r#"
        {
          i = 125; 
          j = 100; 

          while (i - j) 
            if (i < j) 
              j = j - i; 
            else 
              i = i - j;
        }
      "#;

      parse_tc_file(file).unwrap();
    }

    #[test]
    fn do_while_inc_i_by_ten() {
      let file = r#"
        {
          i = 1; 
          do i = i + 10; 
          while ( i < 50 ); 
        }
      "#;

      parse_tc_file(file).unwrap();
    }

    #[test]
    fn while_for_sleeping_a_couple_times() {
      let file = r#"
        {
          i = 1; 
          while ((i = i + 10) < 50) ;
        }
      "#;

      parse_tc_file(file).unwrap();
    }

    #[test]
    fn if_followed_by_if() {
      let file = r#"
        {
          i = 7;
          if (i < 5)
            x = 1;

          if (i < 10)
            y = 2;
        }
      "#;

      parse_tc_file(file).unwrap();
    }

    #[test]
    fn print_ten_times_hello() {
      let file = r#"
        {
          i = 0;
          do {
            i = i + 1;
            print("Hello!");
          } while (i < 10);
        }
      "#;

      parse_tc_file(file).unwrap();
    }
  }

}

#[cfg(test)]
mod rejects {

  use super::super::*;

  #[test]
  #[should_panic]
  fn empty_file() {
    parse_tc_file("").unwrap();      
  }

  #[test]
  #[should_panic]
  fn empty_if() {
    parse_tc_file("if () ;").unwrap();      
  }

  #[test]
  #[should_panic]
  fn empty_if_else() {
    parse_tc_file("if () ; else ;").unwrap();      
  }

  #[test]
  #[should_panic]
  fn empty_do_while() {
    parse_tc_file("do ; while () ;").unwrap();      
  }

  #[test]
  #[should_panic]
  fn empty_while() {
    parse_tc_file("while () ;").unwrap();      
  }

  #[test]
  #[should_panic]
  fn empty_print() {
    parse_tc_file("print () ;").unwrap();      
  }

  #[test]
  #[should_panic]
  fn lone_expression() {
    parse_tc_file("1").unwrap();      
  }

  #[test]
  #[should_panic]
  fn statement_inside_expression() {
    parse_tc_file("(;);").unwrap();      
  }

  mod badly_formed {
    
    use super::super::super::*;

    #[test]
    #[should_panic]
    fn scope() {
      parse_tc_file("{;").unwrap();      
    }

    #[test]
    #[should_panic]
    fn parenthesis() {
      parse_tc_file("(;").unwrap();      
    }
  }

  mod badly_spelled {
    
    use super::super::super::*;

    #[test]
    #[should_panic]
    fn print() {
      parse_tc_file("plint (\"Hello\") ;").unwrap();      
    }

  } 
}
