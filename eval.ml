open Syntax
       
type value =
  IntV of int
| BoolV of bool
| Proc of (value -> value)
| TProc of (unit -> value)
| Tagged of tag * value
and env =
  Empty
| VB of string * value * env
| TB of string * unit ref * env

let rec lookup id = function
    Empty -> failwith ("Variable not found: " ^ id)
  | VB (id', v, env) -> if id = id' then v else lookup id env
  | TB (_, _, env) -> lookup id env

let rec lookupty id = function
    Empty -> failwith ("Tyvar Static or not declared: " ^ id)
  | VB (id', v, env) -> lookupty id env
  | TB (id', v, env) -> if id = id' then v else lookupty id env

let rec tysubstDyn id = function
    Int -> Int
  | Bool -> Bool
  | Dyn -> Dyn
  | Arr(ty1,ty2) -> Arr(tysubstDyn id ty1, tysubstDyn id ty2)
  | Forall(id', ty0) as ty' -> if id = id' then ty' else Forall(id', tysubstDyn id ty0)
  | TyVar id' -> if id = id' then Dyn else TyVar id'
                                                         
let rec eval env = function
    Var id -> lookup id env
  | IConst i -> IntV i
  | BConst b -> BoolV b
  | BinOp(op, e1, e2) ->
     let v1 = eval env e1 in
     let v2 = eval env e2 in
     (match op, v1, v2 with
        Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
      | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
      | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
      | _ -> failwith "Argument not integer")
  | IfExp(e1, e2, e3) ->
     let test = eval env e1 in
     (match test with
        BoolV true -> eval env e2
      | BoolV false -> eval env e3
      | _ -> failwith "If: not bool")
  | FunExp (id, _, e) ->
     Proc (fun v -> eval (VB (id, v, env)) e)
  | AppExp (e1, e2) ->
     let proc = eval env e1 in
     let arg = eval env e2 in
     (match proc with
        Proc f -> f arg
      | _ -> failwith "Not procedure")
  | TSFunExp (id, e) -> TProc (fun () -> eval env e)
  | TGFunExp (id, e) -> TProc
     (fun () -> let r = ref () in eval (TB (id, r, env)) e)
  | TAppExp (e, _) ->
     (match eval env e with
        TProc f -> f ()
      | _ -> failwith "Not a type abstraction")
  | CastExp (e, ty1, ty2) ->
     let v = eval env e in
     (ty1 ==> ty2) env v
and (==>) t1 t2 env = match t1, t2 with  (* cast interpretation *)
    Int, Int -> fun v -> v
  | Arr(Dyn,Dyn), Arr(Dyn,Dyn) -> fun v -> v
  | TyVar id1, TyVar id2 ->
     if id1 = id2 then fun v -> v else failwith ("Not compatible: "^id1^" and "^id2)
  | Dyn, Dyn -> fun v -> v
  | Int, Dyn -> fun v -> Tagged (I, v)
  | Bool, Dyn -> fun v -> Tagged (B, v)
  | Arr(Dyn,Dyn), Dyn -> fun v -> Tagged (Ar, v)
  | TyVar id, Dyn -> fun v -> Tagged (TV (lookupty id env), v)
  | Dyn, (Int | Arr(Dyn,Dyn) | TyVar _ as g) ->
     fun v -> (match v, g with
                 Tagged(I, v0), Int -> v0
               | Tagged(B, v0), Bool -> v0
               | Tagged(Ar, v0), Arr(Dyn, Dyn) -> v0
               | Tagged(Ar, _), Arr(_, _) -> failwith "Can't happen!"
               | Tagged(TV r, v0), TyVar id -> if lookupty id env = r then v0 else failwith "Blame!"
               | Tagged(_, _), _ -> failwith "Blame!"
               | _ -> failwith "Not tagged!")
  | Arr(s1,t1), Arr(s2,t2) ->
     (function
        Proc f -> Proc (fun w -> let arg = (s2 ==> s1) env w in
                                 (t1 ==> t2) env (f arg))
      | _ -> failwith "Not procedure!")
  | Forall(id1, t1), Forall(id2, t2) ->
     (function
        TProc f -> TProc (fun () -> (t1 ==> t2) env (f ()))
      | _ -> failwith "Not polyfun!")
  | ty1, Forall(id2, ty2) ->
     fun v -> TProc (fun () -> (ty1 ==> ty2) (TB(id2, ref (), env)) v)
  | Forall(id1, ty1), ty2 ->
     (function
        TProc f -> (tysubstDyn id1 ty1 ==> ty2) env (f ())  (* Can we avoid this cost of type substitution? *)
      | _ -> failwith "Not polyfun!")
  | Arr(s1,t1) as ty, Dyn ->
     fun v -> Tagged (Ar, (ty ==> Arr(Dyn, Dyn)) env v)
  | Dyn, (Arr(s, t) as ty) ->
     fun v -> (Arr(Dyn,Dyn) ==> ty) env (Tagged (Ar, v))
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

let eval_decl env = function
    Prog e -> let v = eval env e in
              ("-", v, env)
  | Decl(id, ty, e) -> let v = eval env e in
                       (id, v, VB(id, v, env))
