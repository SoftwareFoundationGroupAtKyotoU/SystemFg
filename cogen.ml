open Support.Error
open Syntax
open FC
open Eval

let lift_pos p =
  let open Lexing in
  let {pos_fname=fname; pos_lnum=lnum; pos_bol=bol; pos_cnum=cnum} = p in
  .< {pos_fname=fname; pos_lnum=lnum; pos_bol=bol; pos_cnum=cnum} >.

let errMsg_of_polarity = function
    Pos -> .< fun v -> Printf.sprintf "Blame to the expression side %s" (string_of_val v) >.
  | Neg -> .< fun v -> Printf.sprintf "Blame to the enviroment side %s" (string_of_val v) >.
    
let rec eval = function
    Var(p, idx) -> let p = lift_pos p in fun env -> .< lookup .~p idx .~env >.
  | IConst(_, i) -> fun env -> .< IntV i >.
  | BConst(_, b) -> fun env -> .< BoolV b >.
  | BinOp(p, op, e1, e2) ->
     let v1 = eval e1 in
     let v2 = eval e2 in
     let p = lift_pos p in
     fun env ->
     .< match op, .~(v1 env), .~(v2 env) with
        Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
      | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
      | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
      | _ -> errAt .~p "Can't happen (non-integer argument to binop)">.
  | IfExp(p, e1, e2, e3) ->
     let test = eval e1 in
     let thenclause = eval e2 in
     let elseclause = eval e3 in
     let p = lift_pos p in
     fun env ->
     .< match .~(test env) with
        BoolV true -> .~(thenclause env)
      | BoolV false -> .~(elseclause env)
      | _ -> errAt .~p "Can't happen (nonbool condition)" >.
  | FunExp (_, id, _, e) ->
     let body = eval e in
     fun env -> .< Fun (fun v -> .~(body .< VB (v, .~env) >.)) >.
  | AppExp (p, e1, e2) ->
     let proc = eval e1 in
     let arg = eval e2 in
     let p = lift_pos p in
     fun env ->
     .< match .~(proc env) with
          Fun f -> f .~(arg env)
        | _ -> errAt .~p "Can't happen (application of nonprocedure value)">.
  | TSFunExp (_, id, e) ->
     let body = eval e in  (**** shift -1 ****)
     fun env -> .< TFun (fun () -> .~(body env)) >.
  | TGFunExp (_, id, e) ->
     let body = eval e in
     fun env -> .< TFun (fun () -> let r = ref () in .~(body .< TB (r, .~env) >.)) >.
  | TAppExp (p, e, _) ->
     let tfun = eval e in
     let p = lift_pos p in
     fun env ->
     .< match .~(tfun env) with
          TFun f -> f ()
        | _ -> errAt .~p "Can't happen (application of non-tyabs)" >.
  | CastExp (p, e, ty1, ty2) ->
     let v = eval e in
     let cast = (ty1 ==> ty2) p Pos in
     fun env -> cast env (v env)
and (==>) t1 t2 p plr = match t1, t2 with  (* cast interpretation *)
    Int, Int -> fun env v -> v
  | Arr(Dyn,Dyn), Arr(Dyn,Dyn) -> fun env v -> v
  | TyVar id1, TyVar id2 ->
     if id1 = id2 then fun env v -> v
     else errAt p ("Can't happen: incompatible types "^
                     string_of_int id1^" and "^ string_of_int id2)
  | Dyn, Dyn -> fun env v -> v
  | Int, Dyn -> fun env v -> .< Tagged (I, .~v) >.
  | Bool, Dyn -> fun env v -> .< Tagged (B, .~v) >.
  | Arr(Dyn,Dyn), Dyn -> fun env v -> .< Tagged (Ar, .~v) >.
  | TyVar id, Dyn ->
     let p = lift_pos p in
     fun env v -> .< Tagged (TV (lookupty .~p id .~env), .~v) >.
  | Dyn, Int ->
     let msg = errMsg_of_polarity plr in
     let p = lift_pos p in
     fun env v -> .< match .~v with
                       Tagged(I, v0) -> v0
                     | Tagged(_, _) -> errAt .~p (.~msg .~v)
                     | _ -> errAt .~p "Can't happen (Untagged value)" >.
  | Dyn, Bool ->
     let msg = errMsg_of_polarity plr in
     let p = lift_pos p in
     fun env v -> .< match .~v with
                       Tagged(B, v0) -> v0
                     | Tagged(_, _) -> errAt .~p (.~msg .~v)
                     | _ -> errAt .~p "Can't happen (Untagged value)" >.
  | Dyn, Arr(Dyn,Dyn) ->
     let msg = errMsg_of_polarity plr in
     let p = lift_pos p in
     fun env v -> .< match .~v with
                     | Tagged(Ar, v0) -> v0
                     | Tagged(_, _) -> errAt .~p (.~msg .~v)
                     | _ -> errAt .~p "Can't happen (Untagged value)" >.
  | Dyn, TyVar id ->
     let msg = errMsg_of_polarity plr in
     let p = lift_pos p in
     fun env v -> .< match .~v with
                     | Tagged(TV r, v0) ->
                        if lookupty .~p id .~env == r then v0
                        else errAt .~p (.~msg .~v)
                     | Tagged(_, _) -> errAt .~p (.~msg .~v)
                     | _ -> errAt .~p "Can't happen (Untagged value)" >.
  | Arr(s1,t1), Arr(s2,t2) ->
     let argcast = (s2 ==> s1) p (neg plr) in
     let rescast = (t1 ==> t2) p plr in
     let p = lift_pos p in
     fun env v -> .< match .~v with
                       Fun f -> Fun (fun w -> let arg = .~(argcast env .<w>.) in
                                                .~(rescast env .<f arg>.))
                     | _ -> errAt .~p "Can't happen (Non-procedure value)" >.
  | Forall(id1, t1), Forall(id2, t2) ->
     let bodycast = (t1 ==> t2) p plr in
     let p = lift_pos p in
     fun env v -> .< match .~v with
                       TFun f -> TFun (fun () -> .~(bodycast env .< f () >.))
                     | _ -> errAt .~p "Can't happen (Not polyfun)" >.
  | ty1, Forall(id2, ty2) ->
     let bodycast = (ty1 ==> ty2) p plr in
     fun env v -> .< TFun (fun () -> .~(bodycast .< TB(ref (), .~env) >. v)) >.
  | Forall(id1, ty1), ty2 ->
     let bodycast = (typeInst ty1 Dyn ==> ty2) p plr in
     let p = lift_pos p in
     fun env v -> .< match .~v with
                       TFun f -> .~(bodycast env .< f () >.)
                     | _ -> errAt .~p "Can't happen (Not polyfun)" >.
  | Arr(s1,t1) as ty, Dyn ->
     let cast = (ty ==> Arr(Dyn, Dyn)) p plr in
     fun env v -> .< Tagged (Ar, .~(cast env v)) >.
  | Dyn, (Arr(s, t) as ty) ->
     let cast = (Arr(Dyn,Dyn) ==> ty) p plr in
     fun env v -> cast env .< Tagged (Ar, .~v) >.
  | _, _ -> errAt p "Can't happen!"


let eval_decl env tyenv = function
    Prog e ->
    let code = .< fun env -> .~(eval e .<env>.) >. in
    Print_code.format_code Format.std_formatter (Print_code.close_code code);
    Format.pp_print_newline Format.std_formatter ();
    Format.pp_print_flush Format.std_formatter ();
    (*    let v = Runcode.run code env in *)
    let v = Runnative.run code env in
    ("-", v, env, tyenv)
  | Decl(id, ty, e) ->
     let code = .< fun env -> .~(eval e .<env>.) >. in
     Print_code.format_code Format.std_formatter (Print_code.close_code code);
     Format.pp_print_newline Format.std_formatter ();
     Format.pp_print_flush Format.std_formatter ();
     (*     let v = Runcode.run code env in*)
     let v = Runnative.run code env in
     (id, v, VB(v, env), (id, VDecl ty)::tyenv)
