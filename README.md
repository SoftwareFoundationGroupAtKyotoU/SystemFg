# Interpreter of PBC

## Build instructions

* Do `make depend` and `make`.  You'll get `pbci`.
* If you have BER MetaOCaml, do `make meta` instead of `make`.

## Syntax

* Uppercase IDs for type variables: `'X` (static) and `X` (gradual).
* Lowercase IDs for term variables.
* Types: `Int`, `Bool`, `*` (for type dynamic), `S->T`, `All X.T`.
* Constants: integers, `true`, `false`
* Usual constructs such as: `let x = e1 in e2`, `if e1 then e2 else e3`, `+`, `*`, `<`
* Abstraction: `fun (x : T) -> e`
* Type abstraction: `fun 'X -> e` or `fun X -> e`
* Type application: `e [T]`
* Cast: `(e : S => T)` (parentheses always required)
* Top-level input: `e;;`, `let x : T = e;;`
* a list of parameters allowed for `let` and `fun`, as `fun X (x:X) -> x` or `let id X (x:X) : X = x;;`

## TODO

* Add more test inputs in test.pbc
* `let rec`
* blame labels (with polarity)
* Avoid name conflict with Parser and Lexer in compiler-libs, which BER MetaOCaml uses.  Can we?

## Known bugs

* `pp_ty` doesn't handle variable names declared twice or parentheses very well
