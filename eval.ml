open Support.Error
open Syntax
open FC

type tag = I | B | Ar | TV of unit ref (* aka ground types *)

type value =
  IntV of int
| BoolV of bool
| Fun of (value -> value)
| TFun of (unit -> value)
| Tagged of tag * value
and env =
  Empty
| VB of value * env
| TB of unit ref * env

let rec string_of_val = function
    IntV i -> string_of_int i
  | BoolV true -> "true"
  | BoolV false -> "false"
  | Fun _ -> "<fun>"
  | TFun _ -> "<tfun>"
  | Tagged(I, v) -> Printf.sprintf "%s : Int => *" (string_of_val v)
  | Tagged(B, v) -> Printf.sprintf "%s : Bool => *" (string_of_val v)
  | Tagged(Ar, v) -> Printf.sprintf "%s : *->* => *" (string_of_val v)
  | Tagged(TV _, v) -> Printf.sprintf "%s : X => *" (string_of_val v)
    (* TODO: recover the tyvar name *)

let rec pp_val v = print_string (string_of_val v)

type polarity = Pos | Neg

let neg = function Pos -> Neg | Neg -> Pos

let errMsg_of_polarity plr v tgt = match plr with
    Pos -> Printf.sprintf "Blame on the expression side: %s => %s" (string_of_val v) tgt
  | Neg -> Printf.sprintf "Blame on the enviroment side: %s => %s" (string_of_val v) tgt

let rec lookup pos idx = function
    Empty -> errAt pos ("Can't happen (unbound var : " ^ string_of_int idx ^")")
  | VB (v, env) -> if idx = 0 then v else lookup pos (idx-1) env
  | TB (_, env) -> lookup pos (idx-1) env

let rec lookupty pos idx = function
    Empty -> errAt pos ("Can't happen (unbound tyvar: " ^ string_of_int idx ^ ")")
  | VB (_, env) -> lookupty pos (idx-1) env
  | TB (v, env) -> if idx = 0 then v else lookupty pos (idx-1) env

(* Primitive tag-testing functions *)
let pervasive =
  let open Syntax in
  [("isInt", VDecl (Arr(Dyn,Bool)),
    Fun (fun v -> match v with
                    Tagged(I,_) -> BoolV true
                  | Tagged(TV _,_) -> err "Blame at isInt"
                  | _ -> BoolV false));
   ("isBool", VDecl (Arr(Dyn,Bool)),
    Fun (fun v -> match v with
                    Tagged(B,_) -> BoolV true
                  | Tagged(TV _,_) -> err "Blame at isBool"
                  | _ -> BoolV false));
   ("isFun", VDecl (Arr(Dyn,Bool)),
    Fun (fun v -> match v with
                    Tagged(Ar,_) -> BoolV true
                  | Tagged(TV _,_) -> err "Blame at isFun"
                  | _ -> BoolV false));
  ]
   
let initial_env = List.fold_right (fun (_, _, f) env -> VB(f, env)) pervasive Empty 
let initial_tenv = List.map (fun (s, t, _) -> (s, t)) pervasive
                            
(* eval : term -> env -> value
   (==>) : ty -> ty -> env -> value -> value

 These functions are written in a somewhat peculiar style.  Case
 analysis on the input term or types is performed _before_ it takes an
 environment.  All recursive calls to eval and (==>) are put outside
 "fun env ->".  In some sense, "eval t" and "ty1 ==> ty2" compile t
 and a cast from ty1 to ty2 to an OCaml function of type env -> value
 and env -> value -> value, respectively.  *)

let rec eval = function
    Var(r, idx) -> fun env -> lookup r.frm idx env
  | IConst(_,i) -> fun env -> IntV i
  | BConst(_,b) -> fun env -> BoolV b
  | BinOp(r, op, e1, e2) ->
     let v1 = eval e1 in
     let v2 = eval e2 in
     fun env ->
     (match op, v1 env, v2 env with
        Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
      | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
      | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
      | _ -> errBtw r "Can't happen (non-integer argument to binop)")
  | IfExp(r, e1, e2, e3) ->
     let test = eval e1 in
     let thenclause = eval e2 in
     let elseclause = eval e3 in
     fun env ->
     (match test env with
        BoolV true -> thenclause env
      | BoolV false -> elseclause env
      | _ -> errAt r.frm "Can't happen (nonbool condition)")
  | FunExp (_, id, _, e) ->
     let body = eval e in
     fun env -> Fun (fun v -> body (VB (v, env)))
  | AppExp (r, e1, e2) ->
     let proc = eval e1 in
     let arg = eval e2 in
     fun env ->
     (match proc env with
        Fun f -> f (arg env)
      | _ -> errAt r.frm "Can't happen (application of nonprocedure value)")
  | TSFunExp (_, id, e) ->
     let body = eval e in  (**** shift -1 ****)
     fun env -> TFun (fun () -> body env)
  | TGFunExp (_, id, e) ->
     let body = eval e in
     fun env -> TFun (fun () -> let r = ref () in body (TB (r, env)))
  | TAppExp (r, e, _) ->
     let tfun = eval e in
     fun env ->
     (match tfun env with
        TFun f -> f ()
      | _ -> errAt r.frm "Can't happen (application of non-tyabs")
  | CastExp (r, e, ty1, ty2) ->
     let v = eval e in
     let cast = (ty1 ==> ty2) r Pos in
     fun env -> cast env (v env)
and (==>) t1 t2 r plr = match t1, t2 with  (* cast interpretation *)
    Int, Int -> fun env v -> v
  | Arr(Dyn,Dyn), Arr(Dyn,Dyn) -> fun env v -> v
  | TyVar id1, TyVar id2 ->
     if id1 = id2 then fun env v -> v
     else errAt r.frm ("Can't happen: incompatible types "^
                         string_of_int id1^" and "^ string_of_int id2)
  | Dyn, Dyn -> fun env v -> v
  | Int, Dyn -> fun env v -> Tagged (I, v)
  | Bool, Dyn -> fun env v -> Tagged (B, v)
  | Arr(Dyn,Dyn), Dyn -> fun env v -> Tagged (Ar, v)
  | TyVar id, Dyn -> fun env v -> Tagged (TV (lookupty r.frm id env), v)
  | Dyn, Int ->
     fun env v -> (match v with
                     Tagged(I, v0) -> v0
                   | Tagged(_, _) -> errBtw r (errMsg_of_polarity plr v "Int")
                   | _ -> errAt r.frm "Can't happen (Untagged value)")
  | Dyn, Bool ->
     fun env v -> (match v with
                     Tagged(B, v0) -> v0
                   | Tagged(_, _) -> errBtw r (errMsg_of_polarity plr v "Bool")
                   | _ -> errAt r.frm "Can't happen (Untagged value)")
  | Dyn, Arr(Dyn,Dyn) ->
     fun env v -> (match v with
                   | Tagged(Ar, v0) -> v0
                   | Tagged(_, _) -> errBtw r (errMsg_of_polarity plr v "*->*")
                   | _ -> errAt r.frm "Can't happen (Untagged value)")
  | Dyn, TyVar id ->
     fun env v -> (match v with
                   | Tagged(TV key, v0) ->
                      if lookupty r.frm id env == key then v0
                      else errBtw r (errMsg_of_polarity plr v "Y")
                   | Tagged(_, _) -> errBtw r (errMsg_of_polarity plr v "Z")
                   | _ -> errAt r.frm "Can't happen (Untagged value)")
  | Arr(s1,t1), Arr(s2,t2) ->
     let argcast = (s2 ==> s1) r (neg plr) in
     let rescast = (t1 ==> t2) r plr in
     (fun env -> function
        Fun f -> Fun (fun w -> let arg = argcast env w in
                                 rescast env (f arg))
      | _ -> errAt r.frm "Can't happen (Non-procedure value)")
  | Forall(id1, t1), Forall(id2, t2) ->
     let bodycast = (t1 ==> t2) r plr in
     (fun env -> function
        TFun f -> TFun (fun () -> bodycast env (f ()))
      | _ -> errAt r.frm "Can't happen (Not polyfun)")
  | ty1, Forall(id2, ty2) ->
     let bodycast = (ty1 ==> ty2) r plr in
     fun env v -> TFun (fun () -> bodycast (TB(ref (), env)) v)
  | Forall(id1, ty1), ty2 ->
     let bodycast = (typeInst ty1 Dyn ==> ty2) r plr in
     (fun env -> function
        TFun f -> bodycast env (f ())
      | _ -> errAt r.frm "Can't happen (Not polyfun)")
  | Arr(s1,t1) as ty, Dyn ->
     let cast = (ty ==> Arr(Dyn, Dyn)) r plr in
     fun env v -> Tagged (Ar, cast env v)
  | Dyn, (Arr(s, t) as ty) ->
     let cast = (Arr(Dyn,Dyn) ==> ty) r plr in
     fun env v -> cast env (Tagged (Ar, v))
  | _, _ -> errAt r.frm "Can't happen!"

let eval_decl env tyenv = function
    Prog e -> let v = eval e env in
              ("-", v, env, tyenv)
  | Decl(id, ty, e) -> let v = eval e env in
                       (id, v, VB(v, env), (id, VDecl ty)::tyenv)
