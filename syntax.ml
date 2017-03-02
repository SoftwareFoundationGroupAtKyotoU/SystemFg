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
  
type tyenv = (id * binding) list

let rec name2index ctx id = match ctx with
    [] -> failwith ("Unbound variable: " ^ id)
  | (id', _) :: ctx' -> if id = id' then 0 else (name2index ctx' id) + 1

let rec index2name ctx i = match ctx with
    [] -> failwith ("Index too large: " ^ string_of_int i)
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

let rec pp_ty ctx = function
    Int -> "Int"
  | Bool -> "Bool"
  | Arr(ty1, ty2) -> "(" ^ pp_ty ctx ty1 ^ "->" ^ pp_ty ctx ty2 ^ ")"
  (*     "Arr("^pp_ty ctx ty1^","^pp_ty ctx ty2^")" *)
  | TyVar i -> fst (List.nth ctx i)
  | Forall(id, ty0) -> "(All "^id^". "^pp_ty ((id,STVar)::ctx) ty0^")"
  | Dyn -> "*"
          
type op = Plus | Mult | Lt

module FG =
  struct
    open Lexing
    type term =
      Var of position * int
    | IConst of position * int
    | BConst of position * bool
    | BinOp of position * op * term * term
    | IfExp of position * term * term * term
    | FunExp of position * id * ty * term
    | AppExp of position * term * term
    | TFunExp of position * id * term  (* the body must be a syntactic value and parameter is not needed *)
    | TAppExp of position * term * ty
    | AscExp of position * term * ty (* type ascription *)
    | CastExp of position * term * ty * ty

    type program =
      Prog of term
    | Decl of id * ty * term

    let tmPos = function
        (Var(p, _) | IConst(p, _) | BConst(p, _) | BinOp(p, _, _, _)
         | IfExp(p, _, _, _) | FunExp(p, _, _, _)
         | AppExp(p, _, _) | TFunExp(p, _, _) | TAppExp(p, _, _)
         | AscExp(p, _, _) | CastExp(p, _, _, _))
        -> p
end

module FC =
  struct
    open Lexing
    type term =
      Var of position * int
    | IConst of position * int
    | BConst of position * bool
    | BinOp of position * op * term * term
    | IfExp of position * term * term * term
    | FunExp of position * id * ty * term
    | AppExp of position * term * term
    | TSFunExp of position * id * term  (* the body must be a syntactic value and parameter is not needed *)
    | TGFunExp of position * id * term
    | TAppExp of position * term * ty
    | CastExp of position * term * ty * ty

    type program =
      Prog of term
    | Decl of id * ty * term
                          
    let tmPos = function
        (Var(p, _) | IConst(p, _) | BConst(p, _) | BinOp(p, _, _, _)
         | IfExp(p, _, _, _) | FunExp(p, _, _, _)
         | AppExp(p, _, _) | TSFunExp(p, _, _) | TGFunExp(p, _, _)
         | TAppExp(p, _, _) | CastExp(p, _, _, _))
        -> p
  end
