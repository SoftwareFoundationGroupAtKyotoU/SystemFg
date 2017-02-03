type id = string

type tag = I | B | Ar | TV of unit ref (* aka ground types *)
                      
type ty =
  Int
| Bool
| Arr of ty * ty
| TyVar of id
| Forall of id * ty
| Dyn

type op = Plus | Mult | Lt
                          
type term =
  Var of id
| IConst of int
| BConst of bool
| BinOp of op * term * term
| IfExp of term * term * term
| FunExp of id * ty * term
| AppExp of term * term
| TSFunExp of id * term  (* the body must be a syntactic value and parameter is not needed *)
| TGFunExp of id * term
| TAppExp of term * ty
| CastExp of term * ty * ty 

type program =
  Prog of term
| Decl of id * ty * term
