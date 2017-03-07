open Format
open Syntax

let pr = fprintf

(* generic functions to generate parens depending on precedence *)
let with_paren gt ppf_e e_up ppf e =
  let (>) = gt in
  if e_up > e then pr ppf "(%a)" ppf_e e else pr ppf "%a" ppf_e e

(* if t is the left/only operand of t_up, do you need parentheses for t? *)
let (<) t t_up = match t, t_up with
    (* -> is right associative;
       forall extends to the right as far as possible
    *)
  | (Arr(_,_) | Forall(_,_)),  Arr(_,_) -> true
  | _ -> false

(* if t is the right operand of t_up, do you need parentheses for t? *)
let (>) t_up t = false

let rec print_type ctx ppf t = 
  let with_paren_L = with_paren (fun e_up e -> e < e_up) 
  and with_paren_R = with_paren (>) in
    match t with
        Int -> pp_print_string ppf "Int"
      | Bool -> pp_print_string ppf "Bool"
      | Dyn -> pp_print_string ppf "*"
      | Arr(t1, t2) -> 
         pr ppf "%a -> %a"
            (with_paren_L (print_type ctx) t) t1
            (with_paren_R (print_type ctx) t) t2
      | Forall(id, t0) ->
         pr ppf "All %s%a" id (print_forall ((id,STVar)::ctx)) t0
      | TyVar i -> pp_print_string ppf (fst (List.nth ctx i))
and print_forall ctx ppf t =
  match t with
    Forall(id, t0) -> pr ppf " %s%a" id (print_forall ((id,STVar)::ctx)) t0
  | _ -> pr ppf ". %a" (print_type ctx) t
