open Syntax
open FC
open Eval

let rec eval = function
    Var(_, idx) -> fun env -> .< lookup idx .~env >.
  | IConst(_, i) -> fun env -> .< IntV i >.
  | BConst(_, b) -> fun env -> .< BoolV b >.
  | BinOp(_, op, e1, e2) ->
     let v1 = eval e1 in
     let v2 = eval e2 in
     fun env ->
     .< match op, .~(v1 env), .~(v2 env) with
        Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
      | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
      | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
      | _ -> failwith "Argument not integer">.
  | IfExp(_, e1, e2, e3) ->
     let test = eval e1 in
     let thenclause = eval e2 in
     let elseclause = eval e3 in
     fun env ->
     .< match .~(test env) with
        BoolV true -> .~(thenclause env)
      | BoolV false -> .~(elseclause env)
      | _ -> failwith "If: not bool" >.
  | FunExp (_, id, _, e) ->
     let body = eval e in
     fun env -> .< Fun (fun v -> .~(body .< VB (v, .~env) >.)) >.
  | AppExp (_, e1, e2) ->
     let proc = eval e1 in
     let arg = eval e2 in
     fun env ->
     .< match .~(proc env) with
          Fun f -> f .~(arg env)
        | _ -> failwith "Not procedure" >.
  | TSFunExp (_, id, e) ->
     let body = eval e in  (**** shift -1 ****)
     fun env -> .< TFun (fun () -> .~(body env)) >.
  | TGFunExp (_, id, e) ->
     let body = eval e in
     fun env -> .< TFun (fun () -> let r = ref () in .~(body .< TB (r, .~env) >.)) >.
  | TAppExp (_, e, _) ->
     let tfun = eval e in
     fun env ->
     .< match .~(tfun env) with
          TFun f -> f ()
        | _ -> failwith "Not a type abstraction" >.
  | CastExp (_, e, ty1, ty2) ->
     let v = eval e in
     let cast = (ty1 ==> ty2) in
     fun env -> cast env (v env)
and (==>) t1 t2 = match t1, t2 with  (* cast interpretation *)
    Int, Int -> fun env v -> v
  | Arr(Dyn,Dyn), Arr(Dyn,Dyn) -> fun env v -> v
  | TyVar id1, TyVar id2 ->
     if id1 = id2 then fun env v -> v
     else failwith ("Not compatible: "^ string_of_int id1^" and "^ string_of_int id2)
  | Dyn, Dyn -> fun env v -> v
  | Int, Dyn -> fun env v -> .< Tagged (I, .~v) >.
  | Bool, Dyn -> fun env v -> .< Tagged (B, .~v) >.
  | Arr(Dyn,Dyn), Dyn -> fun env v -> .< Tagged (Ar, .~v) >.
  | TyVar id, Dyn -> fun env v -> .< Tagged (TV (lookupty id .~env), .~v) >.
  | Dyn, Int ->
     fun env v -> .< match .~v with
                       Tagged(I, v0) -> v0
                     | Tagged(_, _) -> failwith "Blame!"
                     | _ -> failwith "Not tagged!" >.
  | Dyn, Bool ->
     fun env v -> .< match .~v with
                       Tagged(B, v0) -> v0
                     | Tagged(_, _) -> failwith "Blame!"
                     | _ -> failwith "Not tagged!" >.
  | Dyn, Arr(Dyn,Dyn) ->
     fun env v -> .< match .~v with
                     | Tagged(Ar, v0) -> v0
                     | Tagged(_, _) -> failwith "Blame!"
                     | _ -> failwith "Not tagged!" >.
  | Dyn, TyVar id ->
     fun env v -> .< match .~v with
                     | Tagged(TV r, v0) ->
                        if lookupty id .~env == r then v0
                        else failwith "Blame!"
                     | Tagged(_, _) -> failwith "Blame!"
                     | _ -> failwith "Not tagged!" >.
  | Arr(s1,t1), Arr(s2,t2) ->
     let argcast = (s2 ==> s1) in
     let rescast = (t1 ==> t2) in
     fun env v -> .< match .~v with
                       Fun f -> Fun (fun w -> let arg = .~(argcast env .<w>.) in
                                                .~(rescast env .<f arg>.))
                     | _ -> failwith "Not procedure!" >.
  | Forall(id1, t1), Forall(id2, t2) ->
     let bodycast = (t1 ==> t2) in
     fun env v -> .< match .~v with
                       TFun f -> TFun (fun () -> .~(bodycast env .< f () >.))
                     | _ -> failwith "Not polyfun!" >.
  | ty1, Forall(id2, ty2) ->
     let bodycast = (ty1 ==> ty2) in
     fun env v -> .< TFun (fun () -> .~(bodycast .< TB(ref (), .~env) >. v)) >.
  | Forall(id1, ty1), ty2 ->
     let bodycast = (typeInst ty1 Dyn ==> ty2) in
     fun env v -> .< match .~v with
                       TFun f -> .~(bodycast env .< f () >.)
                     | _ -> failwith "Not polyfun!" >.
  | Arr(s1,t1) as ty, Dyn ->
     let cast = (ty ==> Arr(Dyn, Dyn)) in
     fun env v -> .< Tagged (Ar, .~(cast env v)) >.
  | Dyn, (Arr(s, t) as ty) ->
     let cast = (Arr(Dyn,Dyn) ==> ty) in
     fun env v -> cast env .< Tagged (Ar, .~v) >.
  | _, _ -> failwith "Can't happen!"


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
