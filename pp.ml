open Format
open Syntax
open Eval

let pr = fprintf

(* generic functions to generate parens depending on precedence *)
let with_paren gt ppf_e e_up ppf e =
  let (>) = gt in
  if e_up > e then pr ppf "(%a)" ppf_e e else pr ppf "%a" ppf_e e

(* if t is the left/only operand of t_up, do you need parentheses for t? *)
let (<) t t_up = match t, t_up with
    (* -> is right associative;
       forall extends to the right as far as possible
       T list is left associative and binds tighter than ->:
    *)
  | (Arr(_,_) | Forall(_,_)), (Arr(_,_) | List _) -> true
  | _ -> false

(* if t is the right operand of t_up, do you need parentheses for t? *)
let (>) t_up t = false

let rec print_type ctx ppf t = 
  let with_paren_L = with_paren (fun e_up e -> e < e_up) 
  and with_paren_R = with_paren (>) in
    match t with
        Int -> pp_print_string ppf "Int"
      | Bool -> pp_print_string ppf "Bool"
      | Dyn -> pp_print_string ppf "?"
      | Arr(t1, t2) -> 
         pr ppf "%a -> %a"
            (with_paren_L (print_type ctx) t) t1
            (with_paren_R (print_type ctx) t) t2
      | Forall(id, t0) ->
         pr ppf "All %s%a" id (print_forall ((id,STVar)::ctx)) t0
      | List t0 ->
         pr ppf "%a list" (with_paren_L (print_type ctx) t) t0
      | TyVar i -> pp_print_string ppf (fst (List.nth ctx i))
and print_forall ctx ppf t =
  match t with
    Forall(id, t0) -> pr ppf " %s%a" id (print_forall ((id,STVar)::ctx)) t0
  | _ -> pr ppf ". %a" (print_type ctx) t

let rec print_val ppf = function
    IntV i -> pp_print_int ppf i
  | BoolV true -> pp_print_string ppf "true"
  | BoolV false -> pp_print_string ppf "false"
  | Fun _ -> pp_print_string ppf "<fun>"
  | TFun _ -> pp_print_string ppf "<tfun>"
  | Tagged(I, v, _) -> pr ppf "%a : Int => ?" print_val v
  | Tagged(B, v, _) -> pr ppf "%a : Bool => ?" print_val v
  | Tagged(Ar, v, _) -> pr ppf "%a : ?->? => ?" print_val v
  | Tagged(L, v, _) -> pr ppf "%a : ? list => ?" print_val v
  | Tagged(TV (_,name), v, _) -> pr ppf "%a : %s => ?" print_val v name
  | NilV | ConsV(_,_) as lst -> pr ppf "[%a]" print_lst lst
and print_lst ppf = function
    NilV -> ()
  | ConsV(v1, NilV) -> pr ppf "%a" print_val v1
  | ConsV(v1, v2) -> pr ppf "%a; %a" print_val v1 print_lst v2
  | v -> raise (ImplBugV (Lexing.dummy_pos, "print_lst: nonlist value", v))
    (* TODO: recover the tyvar name *)
