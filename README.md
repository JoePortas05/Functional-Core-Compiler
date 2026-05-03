# Functional Core (FC) Compiler

FC is a small functional programming language implemented in Racket. The project includes a parser, abstract syntax tree representation, compiler, runtime environment model, closure representation, mutation support, recursive bindings, by-reference function calls, and semantic tests.

The compiler translates parsed FC expressions into executable Racket closures. It supports lexical scoping and resolves local variable bindings at compile time using environment indexes, reducing repeated runtime name lookup.

## Features

- Numeric expressions and identifiers
- Lexical scoping
- First-class functions
- Closures
- Function application
- Conditional expressions
- Local bindings with `bind`
- Recursive bindings with `bindrec`
- Variable mutation with `set!`
- Multi-expression function bodies
- By-reference functions with `rfun`
- Primitive arithmetic and comparison operations
- Semantic tests for closures, recursion, mutation, shadowing, and invalid programs

## Example Programs

### Basic function call

```racket
{{fun {x} {+ x 1}} 4}
```

Result:

```text
5
```

### Closure with lexical scoping

```racket
{bind {{x 3}}
  {bind {{f {fun {y} {+ x y}}}}
    {bind {{x 5}}
      {f 4}}}}
```

Result:

```text
7
```

The function `f` captures the original binding of `x`, demonstrating lexical scoping.

### Recursive function

```racket
{bindrec {{fact {fun {n}
                  {if {= 0 n}
                    1
                    {* n {fact {- n 1}}}}}}}
  {fact 5}}
```

Result:

```text
120
```

### Mutation and closures

```racket
{bind {{make-counter
         {fun {}
           {bind {{c 0}}
             {fun {}
               {set! c {+ 1 c}}
               c}}}}}
  {bind {{c1 {make-counter}}
         {c2 {make-counter}}}
    {* {c1} {c1} {c2} {c1}}}}
```

Result:

```text
6
```

Each counter maintains its own captured mutable state.

### By-reference function call

```racket
{bind {{swap! {rfun {x y}
                {bind {{tmp x}}
                  {set! x y}
                  {set! y tmp}}}}
       {a 1}
       {b 2}}
  {swap! a b}
  {+ a {* 10 b}}}
```

Result:

```text
12
```

The `rfun` form allows function parameters to refer directly to caller-side variable storage.

## Language Syntax

FC programs use S-expression syntax.

```text
<expr> ::= <number>
         | <identifier>
         | { set! <identifier> <expr> }
         | { bind {{ <identifier> <expr> } ... } <expr> <expr> ... }
         | { bindrec {{ <identifier> <expr> } ... } <expr> <expr> ... }
         | { fun { <identifier> ... } <expr> <expr> ... }
         | { rfun { <identifier> ... } <expr> <expr> ... }
         | { if <expr> <expr> <expr> }
         | { <expr> <expr> ... }
```

## Implementation Overview

The implementation is organized around four main components:

### Parsing

The parser converts S-expressions into a typed abstract syntax tree. The AST includes nodes for numbers, identifiers, mutation, local bindings, recursive bindings, functions, by-reference functions, function calls, and conditionals.

### Environments and Values

Runtime environments are represented as nested frames of boxed values. Boxes allow variables to be updated through `set!` and make by-reference function calls possible.

The value model includes:

- Primitive Racket-backed values
- User-defined function closures
- Primitive operations
- Placeholder values used during recursive binding initialization

### Compilation

The compiler converts each AST expression into a Racket function from environment to value. Instead of repeatedly searching variable names at runtime, the compiler resolves local bindings into environment indexes during compilation.

This allows local variables to be accessed by frame index and slot index, similar in spirit to de Bruijn-style environment addressing.

### Function Calls

FC supports both ordinary functions and by-reference functions.

Ordinary functions evaluate arguments into new boxed values before extending the function environment.

By-reference functions require identifier arguments and pass the caller’s existing variable boxes directly, allowing the callee to mutate caller-side variables.

## Running the Project

This project is written in Racket.

To run the compiler and tests, open the main source file in DrRacket or run it with the appropriate Racket command-line environment for the language used by the file.

```bash
racket compiler.rkt
```

Depending on your local Racket setup, you may need the language package that provides `#lang pl`.

## Repository Structure

```text
Functional-Core-Compiler/
  compiler.rkt        # Parser, AST definitions, compiler, runtime, and tests
  README.md           # Project documentation
```
