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
        Int -> pp_print_string ppf "int"
      | Bool -> pp_print_string ppf "bool"
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

let rec print_rawtype ppf t = 
  let with_paren_L = with_paren (fun e_up e -> e < e_up) 
  and with_paren_R = with_paren (>) in
    match t with
        Int -> pp_print_string ppf "int"
      | Bool -> pp_print_string ppf "bool"
      | Dyn -> pp_print_string ppf "?"
      | Arr(t1, t2) -> 
         pr ppf "%a -> %a"
            (with_paren_L print_rawtype t) t1
            (with_paren_R print_rawtype t) t2
      | Forall(id, t0) ->
         pr ppf "All %s%a" id print_rawforall t0
      | List t0 ->
         pr ppf "%a list" (with_paren_L print_rawtype t) t0
      | TyVar i -> pr ppf "#%d" i
and print_rawforall ppf t =
  match t with
    Forall(id, t0) -> pr ppf " %s%a" id print_rawforall t0
  | _ -> pr ppf ". %a" print_rawtype t

let print_op ppf = function
    Plus -> pr ppf " + "
  | Mult -> pr ppf " * "
  | Lt -> pr ppf " < "
  | Eq -> pr ppf " = "

let rec print_rawterm ppf =
  let open Syntax.FG in
  function
    Var (_, i) -> pr ppf "#%d" i
  | IConst (_, i) ->  pr ppf "%d" i
  | BConst (_, true) -> pp_print_string ppf "true"
  | BConst (_, false) -> pp_print_string ppf "false"
  | BinOp (_, op, t1, t2) -> pr ppf "(%a %a %a)" print_rawterm t1 print_op op print_rawterm t2
  | IfExp (_, t1, t2, t3) ->
     pr ppf "if %a then %a else %a" print_rawterm t1 print_rawterm t2 print_rawterm t3
  | FunExp (_, id, ty, t0) ->
     pr ppf "fun (%s : %a) -> %a" id print_rawtype ty print_rawterm t0
  | FixExp (_, id1, id2, ty1, ty2, t0) ->
     pr ppf "fix %s(%s : %a) : %a = %a" id1 id2 print_rawtype ty1 print_rawtype ty2 print_rawterm t0
  | AppExp (_, t1, t2) ->
     pr ppf "(%a) (%a)" print_rawterm t1 print_rawterm t2
  | TFunExp (_, id, t0) ->
     pr ppf "fun %s -> %a" id print_rawterm t0
  | TAppExp (_, t0, ty) ->
     pr ppf "%a [%a]" print_rawterm t0 print_rawtype ty
  | LetExp (_, id, t1, t2) ->
     pr ppf "let %s = %a in %a" id print_rawterm t1 print_rawterm t2
  | AscExp (_, t0, ty) ->
     pr ppf "(%a : %a)" print_rawterm t0 print_rawtype ty
  | CastExp (_, t0, ty1, ty2) ->
     pr ppf "(%a : %a => %a)" print_rawterm t0 print_rawtype ty1 print_rawtype ty2
  | NilExp (_, ty) -> pr ppf "[@%a]" print_rawtype ty
  | ConsExp (_, t1, t2) -> pr ppf "(%a) :: (%a)" print_rawterm t1 print_rawterm t2
  | MatchExp (_, t1, t2, id1, id2, t3) ->
     pr ppf "match %a with [] -> %a | %s::%s -> %a" print_rawterm t1 print_rawterm t2 id1 id2 print_rawterm t3
    
let print_rawdecl ppf = function
    Syntax.FG.Prog t -> print_rawterm ppf t
  | Syntax.FG.Decl (id, t) -> pr ppf "let %s = %a" id print_rawterm t

let print_ctx ppf ctx = List.iter (fun (id,_) -> pr ppf "%s, " id) ctx

module FC =
  struct
    let rec print_rawterm ppf =
      let open Syntax.FC in
      function
        Var (_, i) -> pr ppf "#%d" i
      | IConst (_, i) ->  pr ppf "%d" i
      | BConst (_, true) -> pp_print_string ppf "true"
      | BConst (_, false) -> pp_print_string ppf "false"
      | BinOp (_, op, t1, t2) -> pr ppf "(%a %a %a)" print_rawterm t1 print_op op print_rawterm t2
      | IfExp (_, t1, t2, t3) ->
         pr ppf "if %a then %a else %a" print_rawterm t1 print_rawterm t2 print_rawterm t3
      | (FunExp (_, _, _, _) | TSFunExp (_, _, _) | TGFunExp(_, _, _)) as t ->
         pr ppf "fun %a" print_fun t
      | FixExp (_, id1, id2, ty1, ty2, t0) ->
         pr ppf "fix %s(%s : %a) : %a = %a" id1 id2 print_rawtype ty1 print_rawtype ty2 print_rawterm t0
      | AppExp (_, t1, t2) ->
         pr ppf "(%a) (%a)" print_rawterm t1 print_rawterm t2
      | TAppExp (_, t0, ty) ->
         pr ppf "%a [%a]" print_rawterm t0 print_rawtype ty
      | CastExp (_, t0, ty1, ty2) ->
         pr ppf "(%a : %a => %a)" print_rawterm t0 print_rawtype ty1 print_rawtype ty2
      | NilExp (_, ty) -> pr ppf "[@%a]" print_rawtype ty
      | ConsExp (_, t1, t2) -> pr ppf "(%a) :: (%a)" print_rawterm t1 print_rawterm t2
      | MatchExp (_, t1, t2, id1, id2, t3) ->
         pr ppf "match %a with [] -> %a | %s::%s -> %a" print_rawterm t1 print_rawterm t2 id1 id2 print_rawterm t3
    and print_fun ppf =
      let open Syntax.FC in
      function
      | FunExp (_, id, ty, t0) ->
         pr ppf "(%s : %a) %a" id print_rawtype ty print_fun t0
      | TSFunExp (_, id, t0) ->
         pr ppf "%s %a" id print_fun t0
      | TGFunExp (_, id, t0) ->
         pr ppf "_%s_ %a" id print_fun t0
      | t ->
         pr ppf "-> %a" print_rawterm t
        
    let print_rawdecl ppf = function
        Syntax.FC.Prog t -> print_rawterm ppf t
      | Syntax.FC.Decl (id, t) -> pr ppf "let %s = %a" id print_rawterm t
  end
