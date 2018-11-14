use std::collections::HashSet;
use parser::*;

//////////////////////////////////////
//        Semantic Analysis         //
//////////////////////////////////////

#[allow(dead_code)]
#[derive(Clone)]
struct SymbolTable {
  scopes: Vec<HashSet<String>>,
}

#[allow(dead_code)]
impl SymbolTable {

  fn new() -> SymbolTable {
    SymbolTable {
      Vec::new()
    }
  }

  fn enter_scope(&mut self) {
    self.scopes.push(HashSet::new());
  }

  fn find_symbol(&self, x: &str) -> bool {

    fn find_symbol_in_last_scope(my_scopes: &[HashSet<String>], x: &str) -> bool
    {
      if let Some((last, elements)) = my_scopes.split_last() {
        if last.contains(x) {
          true
        }
        else {
          find_symbol_in_last_scope(elements, x)
        }
      }
      else {
        false
      }
    }

    find_symbol_in_last_scope(&self.scopes, x)
  }

  fn add_symbol(&mut self, x: &str) {
    match self.scopes.last_mut() {
      Some(scope) => scope.insert(x.to_string()),
      None => unreachable!(),
    };
  }

  fn check_scope(&self, x: &str) -> bool {
    match self.scopes.last() {
      Some(scope) => scope.contains(x),
      None => false,
    }
  }

  fn exit_scope(&mut self) {
    self.scopes.pop();
  }
}

#[derive(Debug)]
struct SemanticError {
  variant: ErrorVariant,
}

#[derive(Debug)]
enum ErrorVariant {
  DuplicatedFuncDeclaration,
  NonExistingFunction,
  FuncSignatureMismatch,
}

fn generate_symbol_table(ast: &Statement) -> SymbolTable {

  fn populate_symbol_table(ast: &Statement, &mut SymbolTable) {
    unimplemented!()
  }



   
  unimplemented!()
}

// type SemanticPasses = (FunctionNames);

// fn check_function_names(ast: &Statement) -> FunctionNames {

// }

// fn realize_semantic_passes(ast: &Statement) -> SemanticPasses {
//   unimplemented!()
// }

