use std::collections::HashSet;

//////////////////////////////////////
//        Semantic Analysis         //
//////////////////////////////////////

#[allow(dead_code)]
struct SymbolTable {
  scopes: Vec<HashSet<String>>,
}

#[allow(dead_code)]
impl SymbolTable {

  fn enter_scope(&mut self) {
    self.scopes.push(HashSet::new());
  }

  // THIS is probably the correct way to do things, but it's incomplete.
  // fn find_symbol(&self, x: &str) -> Option<&HashSet> {

  //   fn find_symbol_in_last_scope(hash_slice: &[HashSet<String>], x: &str) -> Option<&HashSet>
  //   {
  //     if let Some((last, elements)) = x.split_last {
  //       if last.contains(x) {
  //         Some(last)
  //       }
  //       else {
  //         find_symbol_in_last_scope(elements)
  //       }
  //     }
  //     else {
  //       None
  //     }
  //   }

  //   find_symbol_in_last_scope(self.scopes)
  // }

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
      None => unreachable!(),
      Some(scope) => scope.insert(x.to_string()),
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

// type SemanticPasses = (FunctionNames);

// fn check_function_names(ast: &Statement) -> FunctionNames {

// }

// fn realize_semantic_passes(ast: &Statement) -> SemanticPasses {
//   unimplemented!()
// }

