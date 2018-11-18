## Cambios al lenguaje original

En esta entrega extendí el lenguaje. Ahora permite usar:

* Funciones (N -> N)
* Comparaciones completas (`<, >, <=, >=, ==`)
* Input de consola (del tipo "a=1, b=100")
* Prints de string literals
* Prints de variables
* Comentarios (de línea y de bloque)

### Tokens

Los patrones nuevos del lenguaje son:

```
// Declaración de funciones
fn double(a) 

// Llamada a funciones
call mul(a, b)

// Nuevas comparaciones
< | > | <= | >= | ==

// String literal
"this is a string"

// Imprimir en consola
print(...)
```

### Gramática

#### EBNF

(OUTDATED) La gramática del lenguaje, en EBNF, es:

```
<program> ::= <statement>
<statement> ::= "if" <paren_expr> <statement> |
                "if" <paren_expr> <statement> "else" <statement> |
                "while" <paren_expr> <statement> |
                "do" <statement> "while" <paren_expr> ";" |
                "{" { <statement>+ } "}" |
                <expr> ";" |
                ";"
<paren_expr> ::= "(" <expr> ")"
<expr> ::= <test> | <id> "=" <expr>
<test> ::= <sum> | <sum> "<" <sum>
<sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term>
<term> ::= <id> | <int> | <paren_expr>
<id> ::= "a" | "b" | "c" | "d" | ... | "z"
<int> ::= <an_unsigned_decimal_integer>
```

#### PEG

La gramática de este lenguaje es una CFG, pero además se puede convertir a una [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar#Expressive_power). Esto nos permite evitar el _backtracking_ a la hora de parsear, y obtener parsing en tiempo lineal usando una librería que se aproveche de esta propiedad.

La gramática final es ésta:

```
////////////////////////////////////////////////
//              Global Settings               //
////////////////////////////////////////////////

//  Whitespace and comments are fitted between every "~"
//  operation (unless the pattern is @ or $, and forcefully if 
//  the pattern is !).

//  That way, they can fill the gaps but can otherwise be ignored
//  for when constructing the AST.

WHITESPACE = _{ " " | "\t" | "\r" | "\n" }

multi_line_comment = _{ "/*" ~ (!"*/" ~ ANY)* ~ "*/" }
single_line_comment = @{ "//" ~ (!NEWLINE ~ ANY)* ~ NEWLINE }

COMMENT = _{ multi_line_comment | single_line_comment }

////////////////////////////////////////////////
//                   Terms                    //
////////////////////////////////////////////////

////// TODO: document the effects of allowing only unsigned integers
////// in the language.

//  Terms are the smallest concept in this grammar.
//  A term can either be:
//   - An expression (nested between two parenthesis)
//   - An integer literal
//   - A variable's id

//  Terms can be evaluated to have a value. In the 
//  case of the "expression" kind of Term, their
//  value is propagated to the left like this:
//   "a = 5"         // Value is 5
//   "1 < 2"         // Value is 1
//   "1 - (2 < 3)"   // Value is 0

int = @{ "0" | ASCII_NONZERO_DIGIT ~ (ASCII_DIGIT)* }

id = { ASCII_ALPHA_LOWER }

term = { paren_expr | id | int }

////////////////////////////////////////////////
//                Operations                  //
////////////////////////////////////////////////

//  Every term in Tiny-C is an integer, and operations 
//  both recieve and deliver integers, accordingly.

//  The effects of a summation and substraction are
//  straightforward.

//  A comparison returns 1 if true, and 0 if false.
//  Accordingly, when using control flow primitives,
//  1 will be true and 0 will be false.

//// TODO: document what truth value have the x > 1

summation = { term ~ "+" ~ sum }
substraction = { term ~ "-" ~ sum }
sum = { summation | substraction | term }

lt = { "<" }
leq = { "<=" }
gt = { ">" }
geq = { ">=" }
eq = { "==" }

comparison = { sum ~ (leq | lt | geq | gt | eq) ~ sum }

////////////////////////////////////////////////
//                Expressions                 //
////////////////////////////////////////////////

//  Expressions can either be assignments, comparisons
//  or arithmetic operations. The key factor
//  that differentiates an expression from a statement,
//  is the fact that expressions can evaluated to a value.

//  For better granularity, expressions can be nested in
//  between parenthesis. This allows them to be treated
//  as individualr terms, which in hand allows them
//  to be part of operations and comparisons.

assignment = { id ~ "=" ~ expr }
expr = !{ assignment | comparison | sum }

// An expression surrounded by parenthesis
paren_expr = { "(" ~ expr ~ ")"}

////////////////////////////////////////////////
//              Print Statements              //
////////////////////////////////////////////////

//  A program can print to Standard Output. There are
//  two options here:
//  - Printing a constant string, defined at compile time.
//  - Printing an integer value, derived from evaluating
//    an expression. 

string = @{ (!(NEWLINE | "\"") ~ ANY)* }

printable = ${ ("\"" ~ string ~ "\"") | expr }

print_statement = { "print" ~ "(" ~ printable ~ ")" ~ ";" }

////////////////////////////////////////////////
//                 Functions                  //
////////////////////////////////////////////////

//  ##  Declaration  ##

//  A program can define functions and call them.
//  Functions have no return value. Instead, they
//  write over the variables they are given as input.

//  If a function were to be given values and not
//  variables as input, its writing operations have
//  no lasting effect.

//  A function may not be given two of the same
//  variable as input, for any writes to that variable
//  would be contradictory.

//  A function's scope has the same set of 26 valid
//  variable identifiers, but they belong to the 
//  function, and are both new and not global.
//  This allows for easier separation of roles,
//  without having global side-effects caused by 
//  function calls.

//  To define a function, you need its name and
//  a list of parameters. The parameters are named
//  as any of the 26 valid identifiers, and cannot 
//  be repeated in the same declaration.

//  In order to return a value, the function overwrites
//  the value of some or all of the variables given
//  to it, by assigning values to them while it is 
//  running.

//  After the function call ends, the return values (if
//  any do apply) will be stored in the variables the 
//  caller gave as input.

//  Tiny-C supports recursion in its functions.

//  ##  Usage  ##

//  Functions are called using the keyword "call",
//  the name of the function, and a list of parameters.

//  The parameters of a function may be either variables
//  or expressions. Variables may be overwritten by the 
//  callee, as that is the way that functions return output.

//  Both the declaration and usage of functions are
//  considered statements, and have therefore no value they
//  can be evaluated to.

function_name = @{ ASCII_ALPHA ~ ASCII_ALPHANUMERIC* }

id_parameter_list = { id ~ (", " ~ id)* }

free_parameter_list = { expr ~ (", " ~ expr)* }

function_declaration_statement = { "fn" ~ function_name ~ "(" ~ id_parameter_list ~ ")" ~ statement }

function_usage_statement = { "call" ~ function_name ~ "(" ~ free_parameter_list ~ ")" ~ ";" }

////////////////////////////////////////////////
//               Control Flow                 //
////////////////////////////////////////////////

//  Control Flow statements allow for branching 
//  flow and conditional repetition of actions.

//  Their usage and syntax is pretty much the 
//  same as in C, with two caveats:

//  - Conditions cannot be combined with && nor ||
//  - Conditions are represented as integers: 
//    x = 0 is False
//    x > 0  is True

if_else = { "if" ~ paren_expr ~ statement ~ "else" ~ statement }
_if = { "if" ~ paren_expr ~ statement }
do_while = { "do" ~ statement ~ "while" ~ paren_expr ~ ";" }
_while = { "while" ~ paren_expr ~ statement }

////////////////////////////////////////////////
//              Other Statements              //
////////////////////////////////////////////////

//  Not all Statements are shiny and special. Most
//  of them are just building blocks from which
//  everything else is built.

//  Other Statements include:

//  - The Scoped Statement, which encloses one or
//    more Statements inside of it (useful for Ifs 
//    and Whiles)
scoped_statement = { "{" ~ statement+ ~ "}" }

//  - The Expression Statement, which encapsulates 
//    an expression, turning it into a statement.
expr_statement = { expr ~ ";" }

//  - The Semicolon Statement, which is a no-op.
semicolon_statement = { ";" }

//  Here's the full list of Statements:
statement = { 
  if_else 
  | _if 
  | do_while 
  | _while 
  | scoped_statement 
  | expr_statement
  | semicolon_statement
  | print_statement
  | function_declaration_statement
  | function_usage_statement
} 

////////////////////////////////////////////////
//            The Language Itself             //
////////////////////////////////////////////////

//  The language itself is just a big statement
//  trapped between the Start of Input and
//  the End of Input.

tiny_c = _{ SOI ~ statement ~ EOI }

```

### Semántica

Buena parte de los errores de escritura son capturados por la gramática. Sin embargo, hay dos tipos de error que no es posible buscar sin pasar al nivel semántico: duplicación de símbolos, y símbolos inalcanzables o no existentes.

#### Símbolos duplicados

No está permitido declarar dos funciones con la misma firma. Por ejemplo:

```c
{
  fn double(a) {
    a = a + a;
  }

  // This triggers an error, because we're redefining an existing symbol.
  fn double(b) {
    b = b + b;
  }

}
```

Sin embargo, si dos funciones difieren en el número de parámetros que aceptan, entonces se consideran funciones distintas:

```c
{
  fn double(a) {
    a = a + a;
  }

  // This is okay. We're defining an entirely different symbol
  fn double(a, b) {
    a = a + a;
    b = b + b;
  }

}
```

Si dos funciones con la misma firma no son alcanzables entre ellas, entonces está todo bien: sus definiciones se aplican a sus scopes respectivos:

```c
{
  {
    fn double(a) {
      a = a + a;
    }
  }

  {
    // This is okay, both symbols belong to different, unconnected spaces.
    fn double(b) {
      b = b + b;
    }
  }

}
```

#### Símbolos inalcanzables y símbolos no existentes

No está permitido llamar a funciones que no existen o que no son visibles desde el punto de llamada. Por ejemplo:


```c
{
  fn double(a) {
    a = a + a;
  }

  // Symbol doesn't exist, therefore this call doesn't make any sense.
  call triple(a);
}
```

También se gatilla un error si el símbolo existe pero no es alcanzable:

```c
{
  {
    fn triple(a) {
      a = a + a + a;
    }
  }

  // Symbol is unreachable from here!
  call triple(a);
}
```

No hay problemas si el símbolo existe en un scope superior, pues es alcanzable:

```c
{
  
  fn triple(a) {
    a = a + a + a;
  }
  
  {
    // Symbol is visible from here; everything's alright.
    call triple(a);
  }
}
```

### UX

#### Feedback de Errores

La experiencia de uso del compilador ha mejorado considerablemente. Ahora los mensajes de error son directos, para los 3 modos de operación:

```console
Running the Tiny-C Compiler...
Verbose mode: on
Reading from input file: code_samples/parse_failures/bad_print.tc
Running in Parser mode
Couldn't parse file.
We think the error is here: 
 --> 2:3
  |
2 |   prink("Hello");
  |   ^---
  |
  = expected lt, leq, gt, geq, or eq
```

```console
Running the Tiny-C Compiler...
Verbose mode: on
Reading from input file: code_samples/sema_failures/duplicated_function.tc
Running Parser mode + Symbol propagation
There seem to be duplicated symbols.
We think the error is here: 
  --> 15:4
   |
15 |    fn double(a) {
   | ...
17 |    }
   |    ^
   |
   = Duplicated symbol: double(1)
```

```console
Running the Tiny-C Compiler...
Verbose mode: on
Reading from input file: code_samples/sema_failures/unreachable_function.tc
Running Parser + Symbol propagation + Symbol reachability analysis
File fails to pass semantic analysis!
We think the error is here: 
  --> 10:3
   |
10 |   call triple(b);
   |   ^-------------^
   |
   = Unreachable symbols: triple(1), 
```
  
#### Formato y captura de información

La información del AST se muestra con indentación apropiada, se muestra el origen en el texto fuente de los nodos del árbol, y el string que capturan. También se muestran los símbolos alcanzables, si corresponde mostrarlos.

```console
Statement {
  span: Span {
    str: "do ; while (i) ;",
    start: 0,
    end: 16
  },
  visible_symbols: {},
  inner: DoWhile(
    Statement {
      span: Span {
        str: ";",
        start: 3,
        end: 4
      },
      visible_symbols: {},
      inner: Empty
    },
    Expression {
      span: Span {
        str: "(i)",
        start: 11,
        end: 14
      },
      inner: Parenthesis(
        Expression {
          span: Span {
            str: "i",
            start: 12,
            end: 13
          },
          inner: Value(
            Sum {
              span: Span {
                str: "i",
                start: 12,
                end: 13
              },
              inner: Tm(
                Term {
                  span: Span {
                    str: "i",
                    start: 12,
                    end: 13
                  },
                  inner: Id(
                    Identifier {
                      span: Span {
                        str: "i",
                        start: 12,
                        end: 13
                      },
                      inner: 'i'
                    }
                  )
                }
              )
            }
          )
        }
      )
    }
  )
}

```

#### Verbose mode

Se implementó el modo verboso de compilación. Por el momento el efecto es superficial, pero sirve para aquellos que gustan de no tener nada que no sea esencial:

```console
Verbose mode: off
File fails to pass semantic analysis!
We think the error is here: 
  --> 16:4
   |
16 |    call triple(b);
   |    ^-------------^
   |
   = Unreachable symbols: triple(1), 
```
