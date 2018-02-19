# Interpreter of PBC

## Requirement

* menhir

## Build instructions

* Do `make depend` and `make`.  You'll get `pbci`.
    * Or, do `omake`.
* __Not maintained right now__: If you have BER MetaOCaml, do `make meta` instead of `make`.

## Syntax

* Uppercase IDs for type variables.
* Lowercase IDs for term variables.
* Types: `int`, `bool`, `?` (for type dynamic), `S->T`, `All X.T`.
    * Nested `All` can be abbreviated, e.g., `All X Y.T` for `All X. All Y. T`.
* Constants: integers, `true`, `false`
* Usual constructs such as: `let x = e1 in e2`, `if e1 then e2 else e3`, `+`, `*`, `<`
* Abstraction: `fun (x : T) -> e`
* Type abstraction: `fun X -> e`
* Type application: `e [T]`
* Recursion: `let rec f (x:S) : T = e in e`
    * The return type annotation `: T` is mandatory.
* Ascription: `(e : T)` (parentheses always required)
* Cast: `(e : S => T)` (parentheses always required)
* Lists: `[@T]` (empty list of `T list`), `e::e`
    * Omitting `@T` makes `? list`
    * `[e; ...; e; @T]` (where "`; @T`" can be omitted) is supported
* Case analysis on lists: `match e with [] -> e | x :: y -> e`
* Top-level input: `e;;`, `let x : T = e;;`, `let f (x:T1) : T = e`, or `let rec f (x:T1) : T = e;;`.
    * Type annotation `:T` is optional in non-recursive definitions.
* A list of parameters allowed for `let` (`let rec`) and `fun`, as in `fun X (x:X) -> x` or `let id X (x:X) : X = x;;`
    * `let rec` takes the form `let rec f X1 ... Xn (x:S) ... : T = e in e` but type abstractions by `Xi` (preceding the first term variable) are done outside of recursion.
        * In other words, this is not polymorphic recursion; the type of `f` is `S -> T` for fixed `Xi`.)

## TODO

* Add more test inputs in test.gtf

## Wish List

* Data structures: pairs
* Implement reduction (not evaluation) for assist proofs
    * pretty printer for Fg- ad Fc-terms

## Known bugs

* `print_type` doesn't handle variable names declared twice properly.
