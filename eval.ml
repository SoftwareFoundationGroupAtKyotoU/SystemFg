open Syntax
       
type value =
  IntV of int
| BoolV of bool
| Proc of (value -> value)
| TProc of (unit -> value)
| Tagged of tag * value
and env =
  Empty
| VB of value * env
| TB of unit ref * env

let rec lookup idx = function
    Empty -> failwith ("lookup: can't happen: " ^ string_of_int idx)
  | VB (v, env) -> if idx = 0 then v else lookup (idx-1) env
  | TB (_, env) -> lookup (idx-1) env

let rec lookupty idx = function
    Empty -> failwith ("lookupty: can't happen: " ^ string_of_int idx)
  | VB (_, env) -> lookupty (idx-1) env
  | TB (v, env) -> if idx = 0 then v else lookupty (idx-1) env

(* eval : term -> env -> value
   (==>) : ty -> ty -> env -> value -> value

 These functions are written in a somewhat peculiar style.  Case
 analysis on the input term or types is performed _before_ it takes an
 environment.  All recursive calls to eval and (==>) are put outside
 "fun env ->".  In some sense, "eval t" and "ty1 ==> ty2" compile t
 and a cast from ty1 to ty2 to an OCaml function of type env -> value
 and env -> value -> value, respectively.  *)
                                                 
let rec eval = function 
    Var idx -> fun env -> lookup idx env
  | IConst i -> fun env -> IntV i
  | BConst b -> fun env -> BoolV b
  | BinOp(op, e1, e2) ->
     let v1 = eval e1 in
     let v2 = eval e2 in
     fun env ->
     (match op, v1 env, v2 env with
        Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
      | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
      | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
      | _ -> failwith "Argument not integer")
  | IfExp(e1, e2, e3) ->
     let test = eval e1 in
     let thenclause = eval e2 in
     let elseclause = eval e3 in
     fun env ->
     (match test env with
        BoolV true -> thenclause env
      | BoolV false -> elseclause env
      | _ -> failwith "If: not bool")
  | FunExp (id, _, e) ->
     let body = eval e in
     fun env -> Proc (fun v -> body (VB (v, env)))
  | AppExp (e1, e2) ->
     let proc = eval e1 in
     let arg = eval e2 in
     fun env ->
     (match proc env with
        Proc f -> f (arg env)
      | _ -> failwith "Not procedure")
  | TSFunExp (id, e) ->
     let body = eval e in  (**** shift -1 ****)
     fun env -> TProc (fun () -> body env)
  | TGFunExp (id, e) ->
     let body = eval e in
     fun env -> TProc (fun () -> let r = ref () in body (TB (r, env)))
  | TAppExp (e, _) ->
     let tfun = eval e in
     fun env ->
     (match tfun env with
        TProc f -> f ()
      | _ -> failwith "Not a type abstraction")
  | CastExp (e, ty1, ty2) ->
     let v = eval e in
     let cast = (ty1 ==> ty2) in
     fun env -> cast env (v env)
and (==>) t1 t2 = match t1, t2 with  (* cast interpretation *)
    Int, Int -> fun env v -> v
  | Arr(Dyn,Dyn), Arr(Dyn,Dyn) -> fun env v -> v
  | TyVar id1, TyVar id2 ->
     if id1 = id2 then fun env v -> v else failwith ("Not compatible: "^ string_of_int id1^" and "^ string_of_int id2)
  | Dyn, Dyn -> fun env v -> v
  | Int, Dyn -> fun env v -> Tagged (I, v)
  | Bool, Dyn -> fun env v -> Tagged (B, v)
  | Arr(Dyn,Dyn), Dyn -> fun env v -> Tagged (Ar, v)
  | TyVar id, Dyn -> fun env v -> Tagged (TV (lookupty id env), v)
  | Dyn, (Int | Arr(Dyn,Dyn) | TyVar _ as g) ->
     fun env v -> (match v, g with
                     Tagged(I, v0), Int -> v0
                   | Tagged(B, v0), Bool -> v0
                   | Tagged(Ar, v0), Arr(Dyn, Dyn) -> v0
                   | Tagged(Ar, _), Arr(_, _) -> failwith "Can't happen!"
                   | Tagged(TV r, v0), TyVar id -> if lookupty id env = r then v0 else failwith "Blame!"
                   | Tagged(_, _), _ -> failwith "Blame!"
                   | _ -> failwith "Not tagged!")
  | Arr(s1,t1), Arr(s2,t2) ->
     let argcast = (s2 ==> s1) in
     let rescast = (t1 ==> t2) in
     (fun env -> function
        Proc f -> Proc (fun w -> let arg = argcast env w in
                                 rescast env (f arg))
      | _ -> failwith "Not procedure!")
  | Forall(id1, t1), Forall(id2, t2) ->
     let bodycast = (t1 ==> t2) in
     (fun env -> function
        TProc f -> TProc (fun () -> bodycast env (f ()))
      | _ -> failwith "Not polyfun!")
  | ty1, Forall(id2, ty2) ->
     let bodycast = (ty1 ==> ty2) in
     fun env v -> TProc (fun () -> bodycast (TB(ref (), env)) v)
  | Forall(id1, ty1), ty2 ->
     let bodycast = (typeInst ty1 Dyn ==> ty2) in
     (fun env -> function
        TProc f -> bodycast env (f ())
      | _ -> failwith "Not polyfun!")
  | Arr(s1,t1) as ty, Dyn ->
     let cast = (ty ==> Arr(Dyn, Dyn)) in
     fun env v -> Tagged (Ar, cast env v)
  | Dyn, (Arr(s, t) as ty) ->
     let cast = (Arr(Dyn,Dyn) ==> ty) in
     fun env v -> cast env (Tagged (Ar, v))
  | _, _ -> failwith "Can't happen!"

let rec pp_val = function
    IntV i -> print_int i
  | BoolV true -> print_string "true"
  | BoolV false -> print_string "false"
  | Proc _ -> print_string "<fun>"
  | TProc _ -> print_string "<tfun>"
  | Tagged(I, v) -> pp_val v; print_string " : Int => ?"
  | Tagged(B, v) -> pp_val v; print_string " : Bool => ?"
  | Tagged(Ar, v) -> pp_val v; print_string " : ?->? => ?"
  | Tagged(TV _, v) -> pp_val v; print_string " : X => ?"

let eval_decl env tyenv = function
    Prog e -> let v = eval e env in
              ("-", v, env, tyenv)
  | Decl(id, ty, e) -> let v = eval e env in
                       (id, v, VB(v, env), (id, VDecl ty)::tyenv)
