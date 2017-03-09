open Support.Error

type id = string

type ty =
  Int
| Bool
| Arr of ty * ty
| TyVar of int
| Forall of id * ty
| Dyn

type binding =
| VDecl of ty
| STVar
| GTVar
| PossiblySTVar of bool ref
| Dummy  (* binding that occurs only during parsing 
            -- After all, name2index is the only function applied to ctx 
               during parsing and it ignores the kind of binding *)

type tyenv = (id * binding) list
exception UnboundVar of Lexing.position * string

let rec name2index ctx id = match ctx with
    [] -> raise (UnboundVar(id.r.frm, "Unbound variable: " ^ id.v))
  | (id', _) :: ctx' -> if id.v = id' then 0 else (name2index ctx' id) + 1

let rec index2name ctx i = match ctx with
    [] -> err ("Index too large: " ^ string_of_int i)
  | (id', _) :: ctx' -> if i = 0 then id' else (index2name ctx' (i-1))

let rec typeShift d i = function  (* shift by d if >= i *)
    Int -> Int
  | Bool -> Bool
  | Arr(t1, t2) -> Arr(typeShift d i t1, typeShift d i t2)
  | TyVar j -> if j >= i then TyVar (j+d) else TyVar j
  | Forall(id, ty) -> Forall(id, typeShift d (i+1) ty)
  | Dyn -> Dyn

let rec typeSubst i ty = function
    Int -> Int
  | Bool -> Bool
  | Arr(ty1, ty2) -> Arr(typeSubst i ty ty1, typeSubst i ty ty2)
  | TyVar j -> if i = j then typeShift i 0 ty else TyVar j
  | Forall(id, ty0) -> Forall(id, typeSubst (i+1) ty ty0)
  | Dyn -> Dyn

let typeInst ty ty' = (* ty must be a body of Forall *)
  typeShift (-1) 0 (typeSubst 0 (typeShift 1 0 ty') ty)

let rec string_of_ty ctx = function
    Int -> "Int"
  | Bool -> "Bool"
  | Arr(ty1, ty2) -> "(" ^ string_of_ty ctx ty1 ^ "->" ^ string_of_ty ctx ty2 ^ ")"
  (*     "Arr("^pp_ty ctx ty1^","^pp_ty ctx ty2^")" *)
  | TyVar i -> fst (List.nth ctx i)
  | Forall(id, ty0) -> "(All "^id^". "^string_of_ty ((id,STVar)::ctx) ty0^")"
  | Dyn -> "*"

type op = Plus | Mult | Lt

module FG =
  struct
    open Lexing
    type term =
      Var of range * int
    | IConst of range * int
    | BConst of range * bool
    | BinOp of range * op * term * term
    | IfExp of range * term * term * term
    | FunExp of range * id * ty * term
    | AppExp of range * term * term
    | TFunExp of range * id * term  (* the body must be a syntactic value and parameter is not needed *)
    | TAppExp of range * term * ty
    | LetExp of range * id * term * term
    | AscExp of range * term * ty (* type ascription *)
    | CastExp of range * term * ty * ty

    type program =
      Prog of term
    | Decl of id * term

    let tmRan = function
        (Var(r, _) | IConst(r, _) | BConst(r, _) | BinOp(r, _, _, _)
         | IfExp(r, _, _, _) | FunExp(r, _, _, _)
         | AppExp(r, _, _) | TFunExp(r, _, _) | TAppExp(r, _, _)
         | LetExp(r, _, _, _) | AscExp(r, _, _) | CastExp(r, _, _, _))
        -> r

    let tmPos t = (tmRan t).frm

    let join_range_exps e1 e2 =
      join_range (tmRan e1) (tmRan e2)
end

module FC =
  struct
    open Lexing
    type term =
      Var of range * int
    | IConst of range * int
    | BConst of range * bool
    | BinOp of range * op * term * term
    | IfExp of range * term * term * term
    | FunExp of range * id * ty * term
    | AppExp of range * term * term
    | TSFunExp of range * id * term  (* the body must be a syntactic value and parameter is not needed *)
    | TGFunExp of range * id * term
    | TAppExp of range * term * ty
    | CastExp of range * term * ty * ty

    type program =
      Prog of term
    | Decl of id * term

    let tmRan = function
        (Var(r, _) | IConst(r, _) | BConst(r, _) | BinOp(r, _, _, _)
         | IfExp(r, _, _, _) | FunExp(r, _, _, _)
         | AppExp(r, _, _) | TSFunExp(r, _, _) | TGFunExp(r, _, _)
         | TAppExp(r, _, _) | CastExp(r, _, _, _))
        -> r

    let tmPos t = (tmRan t).frm

    let join_range_exps e1 e2 =
      join_range (tmRan e1) (tmRan e2)
  end
