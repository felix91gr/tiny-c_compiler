//////////////////////////////////////
//         Usage of Inkwell         //
//////////////////////////////////////

use inkwell::types::BasicTypeEnum;
use inkwell::AddressSpace;
use inkwell::values::IntValue;
use inkwell::values::FloatValue;
use inkwell::IntPredicate;
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;
use std::collections::HashMap;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::builder::Builder;
use inkwell::context::Context;

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

  fn get_own_symbol(&self) -> Option<String> {
    match &self.inner {
      InnerStatement::FnDeclarationStatement(s, identifiers, _) => {
        let symbol = format!("{}({})", s, identifiers.len());
        Some(symbol)
      },
      _ => None,
    }
  }

  fn get_used_symbols(&self) -> HashSet<String> {
    
    let mut used_symbols = HashSet::new();

    match &self.inner {
      InnerStatement::FnUsageStatement(s, exprs) => {
        let symbol = format!("{}({})", s, exprs.len());
        used_symbols.insert(symbol);
      },
      _ => {},
    }

    used_symbols
  }

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

  fn make_semantic_error(&self, message: String) -> Error<Rule> {
    let variant = ErrorVariant::CustomError { message };

    Error::new_from_span(variant, self.span.clone())
  } 

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

  pub fn check_reachability_of_used_symbols(&self) -> Result<String, Error<Rule>> {

    ////// Step 0:
    ////// find current scope
    let mut scope : Vec<&Statement> = Vec::new();

    match self.inner {
      InnerStatement::Scope(ref ss) => {
        for s in ss.iter() {
          scope.push(s);
        }
      },

      InnerStatement::DoWhile(ref s, _) |
      InnerStatement::If(_, ref s) |
      InnerStatement::FnDeclarationStatement(_, _, ref s) |
      InnerStatement::While(_, ref s) => scope.push(s),

      InnerStatement::IfElse(_, ref s1, ref s2) => {
        scope.push(s1);
        scope.push(s2);
      },

      // Empty, Expr, PrintStmt, FnUsageStmt
      _ => {},
    };

    ////// Step 1: 
    ////// Recurse over the interior of this statement 
    for statement in scope.iter() {
      statement.check_reachability_of_used_symbols()?;
    }

    let used_symbols = self.get_used_symbols();

    if used_symbols.is_subset(&self.visible_symbols) {
      Ok("All used symbols are available to this node.".to_string())
    }
    else {
      let mut list_of_missing_symbols = String::new();

      for missing_s in used_symbols.difference(&self.visible_symbols) {
        list_of_missing_symbols.push_str(&format!("{}, ", missing_s));
      }

      Err(self.make_semantic_error(format!("Unreachable symbols: {}", list_of_missing_symbols)))
    }
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

fn print_error(pest_error: Error<Rule>) {
  eprintln!("We think the error is here: \n{}", pest_error);
}

pub fn print_parse_error(pest_error: Error<Rule>) {
  eprintln!("Couldn't parse file.");
  print_error(pest_error);
}

pub fn print_dupe_error(pest_error: Error<Rule>) {
  eprintln!("There seem to be duplicated symbols.");
  print_error(pest_error);
}

pub fn print_sema_error(pest_error: Error<Rule>) {
  eprintln!("File fails to pass semantic analysis!");
  print_error(pest_error);
}

//////////////////////////////////////
//        Compiling to LLVM         //
//////////////////////////////////////

// ======================================================================================
// COMPILER =============================================================================
// ======================================================================================

#[allow(dead_code)]
pub struct Compiler<'a, 'i : 'a> {
    pub context: &'a Context,
    pub builder: &'a Builder,
    pub fpm: &'a PassManager,
    pub module: &'a Module,
    pub statement: &'a Statement<'i>,

    variables: HashMap<String, PointerValue>,
    fn_value_opt: Option<FunctionValue>
}

#[allow(dead_code)]
impl<'a, 'i> Compiler<'a, 'i> {

  fn make_const_int(&self, value: u32) -> IntValue {
    self.context.i32_type().const_int(value as u64, false)
  }

  fn make_void_value(&self) -> IntValue {
    self.context.i32_type().const_null()
  }

  // Done!
  fn compile_term(&self, term: &Term, local_vars: &HashMap<char, PointerValue>) -> Result<IntValue, &'static str> {

    match (*term).inner {
      // Int(u32),
      InnerTerm::Int(value) => Ok(self.make_const_int(value)),

      // Id(Identifier<'i>),
      InnerTerm::Id(ref id) => {
        let var_name = id.inner;

        let ptr_to_local_var = local_vars.get(&var_name).unwrap();
        let loaded_local_variable = self.builder.build_load(*ptr_to_local_var, &format!("deref_{}", var_name));

        Ok(loaded_local_variable.into_int_value())
      },

      // Expr(Box<Expression<'i>>)
      InnerTerm::Expr(ref expr) => self.compile_expr(expr, local_vars),
    }
  }

  // Done!
  fn compile_sum(&self, sum: &Sum, local_vars: &HashMap<char, PointerValue>) -> Result<IntValue, &'static str> {
  
    match (*sum).inner {

      // Tm(Term<'i>),
      InnerSum::Tm(ref term) => self.compile_term(term, local_vars),

      // Summation(Term<'i>, Box<Sum<'i>>),
      InnerSum::Summation(ref term, ref sum) => {

        let lhs = self.compile_term(term, local_vars)?;

        let rhs = self.compile_sum(sum, local_vars)?;

        Ok(self.builder.build_int_add(lhs, rhs, "tmpadd"))

      },

      // Substraction(Term<'i>, Box<Sum<'i>>)
      InnerSum::Substraction(ref term, ref sum) => {

        let lhs = self.compile_term(term, local_vars)?;

        let rhs = self.compile_sum(sum, local_vars)?;

        Ok(self.builder.build_int_sub(lhs, rhs, "tmpsub"))

      },

    }
  }

  // Done!
  fn compile_expr(&self, expr: &Expression, local_vars: &HashMap<char, PointerValue>) -> Result<IntValue, &'static str> {

    match (*expr).inner {

      // Value(Sum<'i>),
      InnerExpression::Value(ref sum) => self.compile_sum(sum, local_vars),

      // Parenthesis(Box<Expression<'i>>),
      InnerExpression::Parenthesis(ref inner_expr) => self.compile_expr(inner_expr, local_vars),

      // Comparison(ComparisonKind, Sum<'i>, Sum<'i>),
      InnerExpression::Comparison(ref comp_kind, ref left_sum, ref right_sum) => {

        /*
          VERY IMPORTANT

          1 = TRUE
          0 = FALSE

          (ANYTHING != 0) = TRUE
        */

        let lhs = self.compile_sum(left_sum, local_vars)?;
        let rhs = self.compile_sum(right_sum, local_vars)?;

        let predicate = match comp_kind {
          ComparisonKind::Equal => IntPredicate::EQ,
          ComparisonKind::LessThanOrEqual => IntPredicate::ULE,
          ComparisonKind::LessThan => IntPredicate::ULT,
          ComparisonKind::GreaterThan => IntPredicate::UGT,
          ComparisonKind::GreaterThanOrEqual => IntPredicate::UGE,
        };

        let comparison = self.builder.build_int_compare(predicate, lhs, rhs, "comp");

        Ok(comparison)
      }

      // Assignment(char, Box<Expression<'i>>),
      InnerExpression::Assignment(var_name, ref rhs_expr) => {

        // A pointer to the local variable to be assigned the rhs value
        let ptr_to_local_var = local_vars.get(&var_name).unwrap();
        
        // The value to be assigned to this variable
        let assigned_value = self.compile_expr(rhs_expr, local_vars)?;

        self.builder.build_store(*ptr_to_local_var, assigned_value);

        // We return the rhs of the assignment, like in C.
        Ok(assigned_value)
      },
    }
  }

  #[allow(unused_variables)]
  fn compile_stmt(&self, local_vars: &HashMap<char, PointerValue>, stmt: &Statement, mother_func: &FunctionValue) -> Result<IntValue, &'static str> {

    let context = &self.context;
    let module  = &self.module;
    let builder = &self.builder;

    let i32_type = context.i32_type();
    let i32_ptr_type = i32_type.ptr_type(AddressSpace::Generic);

    match (*stmt).inner {
      // Empty,
      InnerStatement::Empty => Ok(self.make_void_value()),

      // Expr(Expression<'i>),
      InnerStatement::Expr(ref expr) => { 
        match self.compile_expr(expr, local_vars) {
          Ok(_) => Ok(self.make_void_value()),
          Err(e) => Err(e),
        }
      },

      // IfElse(Expression<'i>, Box<Statement<'i>>, Box<Statement<'i>>),
      InnerStatement::IfElse(ref expr, ref if_true, ref if_false) => {

        // Condition for jumping
        // If condition != 0, then condition == true
        let inner_expr = self.compile_expr(expr, local_vars)?;

        let then_bb = context.append_basic_block(&mother_func, "then");
        let else_bb = context.append_basic_block(&mother_func, "else");
        let cont_bb = context.append_basic_block(&mother_func, "ifcont");

        builder.build_conditional_branch(inner_expr, &then_bb, &else_bb);

        // Build THEN block
        builder.position_at_end(&then_bb);
        self.compile_stmt(local_vars, if_true, mother_func)?;
        builder.build_unconditional_branch(&cont_bb);

        // Build ELSE block
        builder.position_at_end(&else_bb);
        self.compile_stmt(local_vars, if_false, mother_func)?;
        builder.build_unconditional_branch(&cont_bb);

        // Emit MERGE block
        builder.position_at_end(&cont_bb);

        Ok(self.make_void_value())
      },

      // If(Expression<'i>, Box<Statement<'i>>),
      InnerStatement::If(ref expr, ref if_true) => {

        // Condition for jumping
        // If condition != 0, then condition == true
        let inner_expr = self.compile_expr(expr, local_vars)?;

        let then_bb = context.append_basic_block(&mother_func, "then");
        let cont_bb = context.append_basic_block(&mother_func, "ifcont");

        builder.build_conditional_branch(inner_expr, &then_bb, &cont_bb);

        // Build THEN block
        builder.position_at_end(&then_bb);
        self.compile_stmt(local_vars, if_true, mother_func)?;
        builder.build_unconditional_branch(&cont_bb);

        // There is no ELSE block! :D

        builder.position_at_end(&cont_bb);

        Ok(self.make_void_value())
      },
      
      // DoWhile(Box<Statement<'i>>, Expression<'i>),
      InnerStatement::DoWhile(ref action, ref repeat_condition) => {

        let do_block = context.append_basic_block(&mother_func, "do_while_do");
        let check_block = context.append_basic_block(&mother_func, "do_while_check");
        let cont_block = context.append_basic_block(&mother_func, "do_while_cont");

        builder.build_unconditional_branch(&do_block);

        // Build DO block

        builder.position_at_end(&do_block);
        self.compile_stmt(local_vars, action, mother_func)?;
        builder.build_unconditional_branch(&check_block);

        // Build WHILE block

        builder.position_at_end(&check_block);
        let check_condition = self.compile_expr(repeat_condition, local_vars)?;
        builder.build_conditional_branch(check_condition, &do_block, &cont_block);

        builder.position_at_end(&cont_block);

        Ok(self.make_void_value())
      },

      // While(Expression<'i>, Box<Statement<'i>>),
      InnerStatement::While(ref repeat_condition, ref action) => {

        let check_block = context.append_basic_block(&mother_func, "while_check");
        let repeating_block = context.append_basic_block(&mother_func, "while_do");
        let cont_block = context.append_basic_block(&mother_func, "while_cont");

        builder.build_unconditional_branch(&check_block);

        // Build CHECK block

        builder.position_at_end(&check_block);
        let check_condition = self.compile_expr(repeat_condition, local_vars)?;
        builder.build_conditional_branch(check_condition, &repeating_block, &cont_block);

        // Build DO block

        builder.position_at_end(&repeating_block);
        self.compile_stmt(local_vars, action, mother_func)?;
        builder.build_unconditional_branch(&check_block);

        builder.position_at_end(&cont_block);

        Ok(self.make_void_value())
      },

      // Scope(Vec<Statement<'i>>),
      InnerStatement::Scope(ref stmts) => {
        for stmt in stmts {
          self.compile_stmt(local_vars, stmt, mother_func)?;
        }

        Ok(self.make_void_value())
      },
      
      // I hope I can connect to the proper function to do this one
      
      // PrintStatement(Printable<'i>),

      
      // Extra Credits!

      // FnDeclarationStatement(String, Vec<Identifier<'i>>, Box<Statement<'i>>),
      // FnUsageStatement(String, Vec<Expression<'i>>),

      _ => {
        unimplemented!()
      }
    }

  }

  // TODO: add the scope symbols this function has access to!
  fn compile_fn(&self, name: &str, args: &Vec<char>, body: &Statement) -> Result<FunctionValue, &'static str> {

    let context = &self.context;
    let module  = &self.module;
    let builder = &self.builder;

    let i32_type = context.i32_type();
    let i32_ptr_type = i32_type.ptr_type(AddressSpace::Generic);

    let void_type = context.void_type();

    let arg_types = {

      let mut res = Vec::new();

      for _ in args.iter() {
        res.push(i32_ptr_type.into());
      }

      res
    };

    let fn_type = void_type.fn_type(&arg_types[..], false);

    let function = module.add_function(&name.to_string(), fn_type, None);
    let entry = context.append_basic_block(&function, "entry");

    builder.position_at_end(&entry);      

    /*  Current plan:

        - Setup: 1, 2.
        - Play: 3.
        - Cleanup: 4, 5.
        - LLVM's mandatory step: 6.

        1) Allocate space for the 26 variables, set it to 0.

        2) Load the values from the pointers into their corresponding variables.

        3) Compile the body right after (and inside) this function's "entry" block.

        4) Store the values inside the local variables into the pointers.

        5) Return void.

        6) Verify the compiled function, and run the LLVM Pass Manager (optimizer) over it.
    */

    /*
      Stage 1: allocate space for the 26 local variables, and Zero them
    */

    let mut this_fn_vars : HashMap<char, PointerValue> = HashMap::new();

    let zero_value = i32_type.const_int(0, false);

    for var_name in Compiler::valid_variable_names() {
      /* Allocate the variable.                 Parameters: (type, name) */
      let ptr_to_variable = builder.build_alloca(i32_type, &var_name.to_string());
      /* Zero its value.  Parameters: (pointer, value) */
      builder.build_store(ptr_to_variable, zero_value);
      /* Store it into the dictionary of variables */
      this_fn_vars.insert(var_name, ptr_to_variable);
      builder.position_at_end(&entry);
    }

    /*
      Stage 2: load the arguments to the function into the local variables
    */

    for (idx, parameter_pointer) in function.get_params().iter().enumerate() {
      // We get the name of the idx_th argument to the function
      let var_name = args[idx];
      // We look up the pointer to our local variable for that function
      let ptr_to_local_var = this_fn_vars.get(&var_name).unwrap();

      let parameter_pointer = parameter_pointer.as_pointer_value();

      // We load it into a local IntValue
      let loaded_parameter_value = builder.build_load(*parameter_pointer, &format!("deref_{}", var_name));

      // We finally store that IntValue into our local variable :)
      builder.build_store(*ptr_to_local_var, loaded_parameter_value);
      builder.position_at_end(&entry);
    } 

    /*
      Stage 3: compile the function's body
    */

    self.compile_stmt(&this_fn_vars, body, &function)?;

    /*
      Stage 4: store the values in local variables back into the pointers given to us by the function caller
    */

    let exit = function.get_last_basic_block().unwrap();
    
    builder.position_at_end(&exit);

    for (idx, parameter_pointer) in function.get_params().iter().enumerate() {
      // We get the name of the idx_th argument to the function
      let var_name = args[idx];
      // We look up the pointer to our local variable for that function
      let ptr_to_local_var = this_fn_vars.get(&var_name).unwrap();

      let parameter_pointer = parameter_pointer.as_pointer_value();

      // We load our local variable into a local IntValue
      let loaded_parameter_value = builder.build_load(*ptr_to_local_var, &format!("deref_back_to_arg_{}", var_name));

      // We finally store that IntValue into its corresponding outbound pointer :)
      builder.build_store(*parameter_pointer, loaded_parameter_value);
      builder.position_at_end(&exit);
    } 

    /*
      Stage 5: return.
    */

    // This returns void.
    builder.build_return(None);

    /*
      Stage 6: verification and optimization
    */
    if function.verify(true) {
      self.fpm.run_on_function(&function);

      Ok(function)
    } 
    else {

      println!("{:#?}", function);

      unsafe {
          function.delete();
      }
      Err("Invalid generated function.")
    }
  }
  
  fn compile_fn_prototype(&self, name: &str, args: &Vec<char>) -> Result<FunctionValue, &'static str> {
    // Our functions have no return value.
    let ret_type = self.context.void_type();

    // i64 because we expect to be using pointers here.
    let args_types = std::iter::repeat(self.context.i64_type())
      .take(args.len())
      .map(|f| f.into())
      .collect::<Vec<BasicTypeEnum>>();

    let args_types = args_types.as_slice();

    let fn_type = ret_type.fn_type(args_types, false);

    let fn_val = self.module.add_function(&name.to_string(), fn_type, None);

    // set arguments names
    for (i, arg) in fn_val.get_param_iter().enumerate() {
      arg.into_pointer_value().set_name(&args[i].to_string());
    }

    // finally return built prototype
    Ok(fn_val)
  }

  fn compile_main(&self, body: &Statement) -> Result<FunctionValue, &'static str> {

    let name = "main";

    let args = Vec::new();

    self.compile_fn(name, &args, body)
  }

  pub fn compile(context: &'a Context, builder: &'a Builder, pass_manager: &'a PassManager, module: &'a Module, statement: &Statement) -> Result<FunctionValue, &'static str> {
    let compiler = Compiler {
        context: context,
        builder: builder,
        fpm: pass_manager,
        module: module,
        statement: statement,
        fn_value_opt: None,
        variables: HashMap::new()
    };

    compiler.compile_main(statement)
  }

  fn valid_variable_names() -> Vec<char> {
    vec![
      'a',
      'b',
      'c',
      'd',
      'e',
      'f',
      'g',
      'h',
      'i',
      'j',
      'k',
      'l',
      'm',
      'n',
      'o',
      'p',
      'q',
      'r',
      's',
      't',
      'u',
      'v',
      'w',
      'x',
      'y',
      'z']
  }

}

//////////////////////////////////////
//            Unit Tests            //
//////////////////////////////////////

#[cfg(test)]
mod accepts {

  use parser::parse_tc_file;

  #[test]
  fn empty_statement() {
    parse_tc_file(";").unwrap();
  }

  mod basic {

    use parser::parse_tc_file;

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
    use parser::parse_tc_file;
    
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

  use parser::parse_tc_file;

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
    
    use parser::parse_tc_file;

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
    
    use parser::parse_tc_file;

    #[test]
    #[should_panic]
    fn print() {
      parse_tc_file("plint (\"Hello\") ;").unwrap();      
    }

  } 
}
