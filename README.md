# Interpreter of PBC

## Build instructions

* Do `make depend` and `make`.  You'll get `pbci`.
    * Or, do `omake`.
* __Not maintained right now__: If you have BER MetaOCaml, do `make meta` instead of `make`.

## Syntax

* Uppercase IDs for type variables.
* Lowercase IDs for term variables.
* Types: `Int`, `Bool`, `*` (for type dynamic), `S->T`, `All X.T`.
    * Nested `All` can be abbreviated, e.g., `All X Y.T` for `All X. All Y. T`.
* Constants: integers, `true`, `false`
* Usual constructs such as: `let x = e1 in e2`, `if e1 then e2 else e3`, `+`, `*`, `<`
* Abstraction: `fun (x : T) -> e`
* Type abstraction: `fun X -> e`
* Type application: `e [T]`
* Recursion: `let rec f (x:T1) : T = e in e`
    * The return type annotation `: T` is mandatory.
* Ascription: `(e : T)` (parentheses always required)
* Cast: `(e : S => T)` (parentheses always required)
* Top-level input: `e;;`, `let x : T = e;;`, `let f (x:T1) : T = e`, or `let rec f (x:T1) : T = e;;`.
    * Type annotation `:T` is optional in non-recursive definitions.
* A list of parameters allowed for `let` (`let rec`) and `fun`, as in `fun X (x:X) -> x` or `let id X (x:X) : X = x;;`
    * The first parameter for `let rec` must be a term variable.

## TODO

* Add more test inputs in test.gtf

## Wish List

* Data structures: pairs and lists
* Implement reduction (not evaluation) for assist proofs
    * pretty printer for Fg- ad Fc-terms

## Known bugs

* `print_type` doesn't handle variable names declared twice properly.
