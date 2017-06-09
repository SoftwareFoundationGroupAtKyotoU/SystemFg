open Support.Error
open Syntax
open FC

type tag = I | B | Ar | L | TV of unit ref * id (* aka ground types *)

type value =
  IntV of int
| BoolV of bool
| Fun of (value -> value)
| TFun of (unit -> value)
| NilV
| ConsV of value * value
| Tagged of tag * value * range
and env =
  Empty
| VB of value * env
| TB of unit ref * id * env  (* id for blame message *)

type polarity = Pos | Neg

let neg = function Pos -> Neg | Neg -> Pos

exception ImplBug of Lexing.position * string
exception ImplBugV of Lexing.position * string * value
exception ImplBugRanV of range * string * value
exception Blame of range * polarity * value * string

let rec lookup idx = function
    Empty -> raise Not_found
  | VB (v, env) -> if idx = 0 then v else lookup (idx-1) env
  | TB (_, _, env) -> lookup (idx-1) env

let lookup pos idx env =
  try lookup idx env with
    Not_found -> raise (ImplBug (pos, "unbound var : " ^ string_of_int idx))

let rec lookupty idx = function
    Empty -> raise Not_found
  | VB (_, env) -> lookupty (idx-1) env
  | TB (v, name, env) -> if idx = 0 then (v, name) else lookupty (idx-1) env

let lookupty pos idx env =
  try lookupty idx env with
    Not_found -> raise (ImplBug (pos, "unbound tyvar: " ^ string_of_int idx))

(* Primitive tag-testing functions *)
exception Untagged
let pervasive =
  let open Syntax in
  [("isInt", VDecl (Arr(Dyn,Bool)),
    Fun (fun v -> match v with
                    Tagged(I,_,_) -> BoolV true
                  | Tagged(TV (_,id),_,_) -> failwith "isInt"
                  | Tagged(_, _,_) -> BoolV false
                  | _ -> raise Untagged));
   ("isBool", VDecl (Arr(Dyn,Bool)),
    Fun (fun v -> match v with
                    Tagged(B,_,_) -> BoolV true
                  | Tagged(TV (_,id),_,_) -> failwith "isBool"
                  | Tagged(_,_,_) -> BoolV false
                  | _ -> raise Untagged));
   ("isFun", VDecl (Arr(Dyn,Bool)),
    Fun (fun v -> match v with
                    Tagged(Ar,_,_) -> BoolV true
                  | Tagged(TV (_,id),_,_) -> failwith "isFun"
                  | Tagged(_,_,_) -> BoolV false
                  | _ -> raise Untagged))
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
      | _, v1, IntV _ -> raise (ImplBugRanV (r, "non-integer 1st argument to binop", v1))
      | _, _, v2 -> raise (ImplBugRanV (r, "non-integer 2nd argument to binop", v2)))
  | IfExp(r, e1, e2, e3) ->
     let test = eval e1 in
     let thenclause = eval e2 in
     let elseclause = eval e3 in
     fun env ->
     (match test env with
        BoolV true -> thenclause env
      | BoolV false -> elseclause env
      | v -> raise (ImplBugV (r.frm, "nonbool condition", v)))
  | FunExp (_, id, _, e) ->
     let body = eval e in
     fun env -> Fun (fun v -> body (VB (v, env)))
  | FixExp (_, id1, id2, _, _, e) ->
     let body = eval e in
     fun env ->
     let rec f v = body (VB (v, VB (Fun f, env))) in
     Fun f
  | AppExp (r, e1, e2) ->
     let proc = eval e1 in
     let arg = eval e2 in
     fun env ->
     (match proc env with
        Fun f -> let arg = arg env in
                 (try f arg with
                    Untagged ->
                      raise (ImplBugV (tmPos e2, "untagged value", arg))
                  | Failure s -> raise (Blame (r, Pos, arg, s)))
      | v -> raise (ImplBugV (r.frm, "application of nonprocedure value", v)))
  | TSFunExp (_, id, e) ->
     let body = eval e in  (**** shift -1 ****)
     fun env -> TFun (fun () -> body env)
  | TGFunExp (_, id, e) ->
     let body = eval e in
     fun env -> TFun (fun () -> let r = ref () in body (TB (r, id, env)))
  | TAppExp (r, e, _) ->
     let tfun = eval e in
     fun env ->
     (match tfun env with
        TFun f -> f ()
      | v -> raise (ImplBugV (r.frm, "application of non-tyabs", v)))
  | CastExp (r, e, ty1, ty2) ->
     let v = eval e in
     let cast = (ty1 ==> ty2) r Pos in
     fun env -> cast env (v env)
  | NilExp (r, ty) -> fun env -> NilV
  | ConsExp (r, e1, e2) ->
     let v1 = eval e1 in
     let v2 = eval e2 in
     fun env -> ConsV(v1 env, v2 env)
  | MatchExp (r, e0, e1, _, _, e2) ->
     let v0 = eval e0 in
     let v1 = eval e1 in
     let v2 = eval e2 in
     (fun env -> match v0 env with
                   NilV -> v1 env
                 | ConsV(v21, v22) -> v2 (VB (v22, VB(v21, env)))
                 | v -> raise (ImplBugV (r.frm, "match on non-list", v)))
and (==>) t1 t2 r plr = match t1, t2 with  (* cast interpretation *)
    Int, Int -> (* R_Id *)
     fun env v -> v
  | Bool, Bool -> (* R_Id *)
     fun env v -> v
  | Arr(Dyn,Dyn), Arr(Dyn,Dyn) -> (* R_Id *)
     fun env v -> v
  | List Dyn, List Dyn -> (* R_Id *)
     fun env v -> v
  | TyVar id1, TyVar id2 -> (* R_Id *)
     if id1 = id2 then fun env v -> v
     else raise (ImplBug (r.frm, ("incompatible types "^string_of_int id1^" and "^ string_of_int id2)))
  | Dyn, Dyn -> (* R_Idstar *)
     fun env v -> v
  | Int, Dyn ->
     fun env v -> Tagged (I, v, r)
  | Bool, Dyn ->
     fun env v -> Tagged (B, v, r)
  | Arr(Dyn,Dyn), Dyn ->
     fun env v -> Tagged (Ar, v, r)
  | List Dyn, Dyn ->
     fun env v -> Tagged (L, v, r)
  | TyVar idx, Dyn ->
     fun env v -> let (key,name) = lookupty r.frm idx env in
                  Tagged (TV (key,name), v, r)
  | Dyn, Int -> (* R_Collapse/R_Conflict *)
     fun env v -> (match v with
                     Tagged(I, v0, _) -> v0
                   | Tagged(_, _, _) -> raise (Blame (r, plr, v, "Int"))
                   | _ -> raise (ImplBugV (r.frm, "untagged value", v)))
  | Dyn, Bool -> (* R_Collapse/R_Conflict *)
     fun env v -> (match v with
                     Tagged(B, v0, _) -> v0
                   | Tagged(_, _, _) -> raise (Blame (r, plr, v, "Bool"))
                   | _ -> raise (ImplBugV (r.frm, "untagged value", v)))
  | Dyn, Arr(Dyn,Dyn) -> (* R_Collapse/R_Conflict *)
     fun env v -> (match v with
                   | Tagged(Ar, v0, _) -> v0
                   | Tagged(_, _, _) -> raise (Blame (r, plr, v, "*->*"))
                   | _ -> raise (ImplBugV (r.frm, "untagged value", v)))
  | Dyn, List Dyn -> (* R_Collapse/R_Conflict *)
     fun env v -> (match v with
                   | Tagged(L, v0, _) -> v0
                   | Tagged(_, _, _) -> raise (Blame (r, plr, v, "* list"))
                   | _ -> raise (ImplBugV (r.frm, "untagged value", v)))
  | Dyn, TyVar id -> (* R_Collapse/R_Conflict *)
     fun env v -> (match v with
                   | Tagged(TV (key1,name), v0, _) ->
                      let (key2,name) = lookupty r.frm id env in
                      if key2 == key1 then v0
                      else raise (Blame (r, plr, v, name))
                   | Tagged(_, _, _) -> raise (Blame (r, plr, v, "Z"))
                   | _ ->  raise (ImplBugV (r.frm, "untagged value", v)))
  | Arr(s1,t1), Arr(s2,t2) -> (* R_Wrap *)
     let argcast = (s2 ==> s1) r (neg plr) in
     let rescast = (t1 ==> t2) r plr in
     (fun env -> function
        Fun f -> Fun (fun w -> let arg = argcast env w in
                                 rescast env (f arg))
      | v -> raise (ImplBugV (r.frm, "nonprocedural value", v)))
  | List s0, List t0 ->
     let elmcast = (s0 ==> t0) r plr in
     (let rec loop env = function
         NilV -> NilV
       | ConsV(v1, v2) -> ConsV(elmcast env v1, loop env v2)
       | v -> raise (ImplBugV (r.frm, "nonlist value", v))
      in loop)
  | Forall(id1, t1), Forall(id2, t2) -> (* R_Content *)
     let bodycast = (t1 ==> t2) r plr in
     (fun env -> function
        TFun f -> TFun (fun () -> bodycast env (f ()))
      | v -> raise (ImplBugV (r.frm, "application of non-tyabs", v)))
  | ty1, Forall(id2, ty2) -> (* R_Generalize *)
     let bodycast = (typeShift 1 0 ty1 ==> ty2) r plr in
     fun env v -> TFun (fun () -> bodycast (TB (ref (), id2, env)) v)
  | Forall(id1, ty1), ty2 -> (* R_Instantiate *)
     let bodycast = (typeInst ty1 Dyn ==> ty2) r plr in
     (fun env -> function
        TFun f -> bodycast env (f ())
      | v ->  raise (ImplBugV (r.frm, "application of non-tyabs", v)))
  | Arr(s1,t1) as ty, Dyn -> (* R_Ground *)
     let cast = (ty ==> Arr(Dyn, Dyn)) r plr in
     fun env v -> Tagged (Ar, cast env v, r)
  | Dyn, Arr(s, t) -> (* R_Extend *)
     let cast1 = (Arr(Dyn,Dyn) ==> t2) r plr in
     let cast2 = (Dyn ==> Arr(Dyn,Dyn)) r plr in
     fun env v -> cast1 env (cast2 env v)
  | List s0, Dyn ->
     let cast = (t1 ==> List Dyn) r plr in
     fun env v -> Tagged(L, cast env v, r)
  | Dyn, List t0 ->
     let cast1 = (List Dyn ==> t2) r plr in
     let cast2 = (Dyn ==> List Dyn) r plr in
     fun env v -> cast1 env (cast2 env v)
  | _, _ -> raise (ImplBug (r.frm, "non-compatible types encountered in (==>)"))
