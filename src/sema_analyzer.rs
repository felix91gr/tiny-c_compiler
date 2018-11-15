// use std::collections::HashSet;
// use parser::*;

// //////////////////////////////////////
// //        Semantic Analysis         //
// //////////////////////////////////////

// #[allow(dead_code)]
// #[derive(Clone)]
// struct SymbolTable {
//   scopes: Vec<HashSet<String>>,
// }

// #[allow(dead_code)]
// impl SymbolTable {

//   fn new() -> SymbolTable {
//     SymbolTable {
//       Vec::new()
//     }
//   }

//   fn enter_scope(&mut self) {
//     self.scopes.push(HashSet::new());
//   }

//   fn find_symbol(&self, x: String) -> bool {

//     fn find_symbol_in_last_scope(my_scopes: &[HashSet<String>], x: String) -> bool
//     {
//       if let Some((last, elements)) = my_scopes.split_last() {
//         if last.contains(x) {
//           true
//         }
//         else {
//           find_symbol_in_last_scope(elements, x)
//         }
//       }
//       else {
//         false
//       }
//     }

//     find_symbol_in_last_scope(&self.scopes, x)
//   }

//   fn add_symbol(&mut self, x: String) {
//     match self.scopes.last_mut() {
//       Some(scope) => scope.insert(x),
//       None => unreachable!(),
//     };
//   }

//   fn check_scope(&self, x: String) -> bool {
//     match self.scopes.last() {
//       Some(scope) => scope.contains(x),
//       None => false,
//     }
//   }

//   fn exit_scope(&mut self) {
//     self.scopes.pop();
//   }

//   fn get_all_visible_symbols(&self) -> HashSet<String> {
//     unimplemented!()
//   }
// }

// fn enrich_ast(ast: &mut Statement) {
  
//   let mut symbol_table = SymbolTable::new();

//   match expr {
//     Some(expr) => expr,
//     None => expr,
//   }

//   unimplemented!()
// }

// fn populate_with_symbols(statement: &mut Statement, symbol_table: &mut SymbolTable) {
//   statement.add_visible_symbols(symbol_table.get_all_visible_symbols());

//   if let Some(symbol) = statement.get_own_symbol() {
//     symbol_table.add_symbol(symbol);
//   }

  
// }

// fn populate_with_symbols(statements: Vec<&mut Statement>, symbol_table: &mut SymbolTable) {

//   for statement in statements.iter() {
//     statement.add_visible_symbols(symbol_table.get_all_visible_symbols());
//   }

//   for statement in statements.iter() {
//     if let Some(symbol) = statement.get_own_symbol() {
//       symbol_table.add_symbol(symbol);
//     }
//   }

//   for statement in statements.iter() {

//   }

//   unimplemented!()
// }