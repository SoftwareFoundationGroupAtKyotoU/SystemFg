# Interpreter of PBC

## Syntax

* Uppercase IDs for type variables: `'X` (static) and `X` (gradual).
* Lowercase IDs for term variables.
* Types: `Int`, `Bool`, `S->T`, `All X.T`.
* Constants: integers, `true`, `false`
* Usual constructs such as: `let x = e1 in e2`, `if e1 then e2 else e3`, `+`, `*`, `<`
* Abstraction: `fun (x : T) -> e`
* Type abstraction: `fun X -> e` or `fun 'X -> e`
* Type application: `e [T]`
* Cast: `(e : S => T)` (parentheses always required)

## TODO

* Typechecking (using de Bruijn indices?)
* blame labels (with polarity)
* staging?
