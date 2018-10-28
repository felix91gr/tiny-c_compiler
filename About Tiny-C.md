## Lenguaje a compilar: Tiny-C

Tiny-C es un subconjunto muy pequeño de C.

Las variables "a" a la "z" son globales, predefinidas e inicializadas a 0. No es posible declarar nuevas variables. Al ejecutar un programa, éste imprime el valor de las variables que no son 0 al finalizar.

El lenguaje no permite crear funciones ni recibir input externo. El control de flujo se hace mediante `while`, `do-while`, `if`, `else-if` y scopes (espacio entre 2 llaves: `{...}`.

Este lenguaje es claramente muy pequeño, por lo que planeo expandirlo una vez la base esté firme. Idealmente, le agregaría:

* Funciones (0..N) -> 1
* Comparaciones completas (por el momento sólo acepta "<")
* Variables del usuario
* Input de consola (del tipo "a=1, b=100")
* Arreglos, quizás.

Mediante testing planeo evitar romper lo que ya funciona. Así debiera poder ir creciendo el lenguaje de a poco en lugar de construir un lenguaje grande desde 0.

### Tokens

Los tokens del lenguaje son:

```
// Constantes
int = 1


// Variables
id = a

// Un término
term = (a)

// Suma
summation = i + j
// Resta
substraction = 1 - a
// Un valor numérico
sum = (a - 1) + 2

// Una comparación (1 == true, other == false)
comparison = a < 2

// Una asignación sobre una variable
assignment = i = 1
// Una expresión. Expresiones siempre tienen valores numéricos
expr = i = j < (2 - k)

// Una expresión entre paréntesis
paren_expr = (i = j < (2 - k))

// Statements:
// Par if-else
if_else = if (i < j) { i = j; } else { j = i }
// If
_if = if (i < j) { i = j; }

// Do While
do_while = do {;} while(1);
// While
_while = while(1) ;

// Un Scope con 1 o más Statements
scoped_statement = { a = b = 1; }

// Una expresión dentro de un Statement
expr_statement = (((a = 1)));

// Un Statement vacío
semicolon_statement = ;

// Cualquier Statement
statement = { i = j; }

// Un programa
tiny_c = do ; while (i) ;
```

### Gramática

La gramática del lenguaje, en EBNF, es:

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

Esta gramática es una CFG, pero además se puede convertir a una [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar#Expressive_power). Esto nos permite evitar el _backtracking_ a la hora de parsear, y obtener parsing en tiempo lineal usando una librería que se aproveche de esta propiedad.

La gramática final es ésta:

```
// Global settings
WHITESPACE = _{ " " | "\t" | "\r" | "\n" }

// Positive integers
int = @{ "0" | ASCII_NONZERO_DIGIT ~ (ASCII_DIGIT)* }

// Lower letters are the only variables, and they are global.
id = { ASCII_ALPHA_LOWER }

// Expressions are always terms, as in C: "a = 5" has the value "5"
term = { paren_expr | id | int }

// Everything is a number here, therefore terms are already sums
summation = { term ~ "+" ~ sum }
substraction = { term ~ "-" ~ sum }
sum = { summation | substraction | term }

// True if "sum" is different from 0, or if $1 < $2
comparison = { sum ~ "<" ~ sum }

// An expression
assignment = { id ~ "=" ~ expr }
expr = { assignment | comparison | sum }

// An expression surrounded by parenthesis
paren_expr = { "(" ~ expr ~ ")"}

// All the different Statements
if_else = { "if" ~ paren_expr ~ statement ~ "else" ~ statement }
_if = { "if" ~ paren_expr ~ statement }
do_while = { "do" ~ statement ~ "while" ~ paren_expr ~ ";" }
_while = { "while" ~ paren_expr ~ statement }
scoped_statement = { "{" ~ statement+ ~ "}" }
expr_statement = { expr ~ ";" }
semicolon_statement = { ";" }

//
statement = { 
	if_else 
	| _if 
	| do_while 
	| _while 
	| scoped_statement 
	| expr_statement
	| semicolon_statement
} 

// The language
tiny_c = _{ SOI ~ statement ~ EOI }

```

Muy importante es el orden de las opciones de cada regla, como por ejemplo para `expr`:


`expr = { assignment | comparison | sum }`

Si fuese 

`expr = { sum | assignment | comparison }`

Entonces el parser terminaría antes de parsear todo el archivo en casos como:

`i = 5`

Pues antes de encontrar el resultado de `assignment`, encontraría `sum -> term -> i`. Y al no poder expandir más el match ni retroceder, falla el parseo.

Algo similar pasa al invertir el orden de `if` y `if_else` o de `while` y `do_while`:

```
statement = { 
	_if 
	| if_else 
	| _while 
	| do_while 
	| scoped_statement 
	| expr_statement
	| semicolon_statement
} 
```

Sucede que al encontrar un `if_else`, primero lo parsea como un `if` y luego no sabe qué hacer con el `else` y falla.

Como lección de todo esto, podemos obtener que las PEG son muy útiles pero es necesario tener cuidado de que (1) podamos tener una gramática completa que no retroceda y (2) de poder reordenar bien sus reglas cuando todavía esté tratando de retroceder.