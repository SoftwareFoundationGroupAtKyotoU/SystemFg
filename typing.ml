open Support.Error
open Syntax

let rec isStatic ctx i =
  match List.nth ctx i with
    (_, STVar) -> true
  | (_, GTVar) -> false
  | (_, PossiblySTVar f) -> !f
  | (id, _) -> err ("isStatic: cannot happen -- " ^ id)

let rec isGradual ctx i =
  match List.nth ctx i with
    (_, STVar) -> false
  | (_, GTVar) -> true
  | (_, PossiblySTVar f) -> not !f
  | (id, _) -> err ("isGradual: cannot happen -- " ^ id)

let rec merge l1 l2 =
  match l1, l2 with
    [], _ -> l2
  | _, [] -> l1
  | i1::l1', i2::l2' when i1 = i2 -> i1 :: merge l1 l2'
  | i1::l1', i2::l2' when i1 < i2 -> i1 :: i2 :: merge l1 l2'
  | i1::l1', i2::l2' -> i2 :: i1 :: merge l1 l2'

let rec shiftTVs d = function
    [] -> []
  | i::l' -> let i' = i-d in
             if i' < 0 then shiftTVs d l'
             else i' :: shiftTVs d l'

let rec forall p = function
    [] -> true
  | x::l -> p x && forall p l

let rec freeTVs i = function
    Int -> []
  | Bool -> []
  | Arr(ty1, ty2) -> merge (freeTVs i ty1) (freeTVs i ty2)
  | TyVar j -> if j >= i then [j] else []
  | Forall(_, ty0) -> shiftTVs (-1) (freeTVs (i+1) ty0)
  | List ty0 -> freeTVs i ty0
  | Dyn -> []

let freeTVs = freeTVs 0

let rec containsDyn = function
    Int -> false
  | Bool -> false
  | Arr(ty1, ty2) -> containsDyn ty1 || containsDyn ty2
  | TyVar _ -> false
  | Forall(_, ty0) -> containsDyn ty0
  | List ty0 -> containsDyn ty0
  | Dyn -> true

let rec precon isG ctx ty1 ty2 =
  ty1 = ty2 ||
    match ty1, ty2 with
      Arr(tys1, tyt1), Arr(tys2, tyt2) ->
      precon isG ctx tys1 tys2 && precon isG ctx tyt1 tyt2
    | List ty10, List ty20 -> precon isG ctx ty10 ty20
    | Forall(id, ty1'), Forall(_, ty2') ->
       precon isG ((id,STVar)::ctx) ty1' ty2'
    | Forall(id, ty1'), _ ->
       containsDyn ty2 && precon isG ((id,GTVar)::ctx) ty1' (typeShift 1 0 ty2)
    | _, Forall(id, ty2') ->
       containsDyn ty1 && precon isG ((id,GTVar)::ctx) (typeShift 1 0 ty1) ty2'
    | Dyn, _ -> forall (isG ctx) (freeTVs ty2)
    | _, Dyn -> forall (isG ctx) (freeTVs ty1)
    | _ -> false

let rec join ctx ty1 ty2 =
  match ty1, ty2 with
    Dyn, _ -> if forall (isGradual ctx) (freeTVs ty2) then Some Dyn else None
  | _, Dyn -> if forall (isGradual ctx) (freeTVs ty1) then Some Dyn else None
  | Int, Int -> Some Int
  | Bool, Bool -> Some Bool
  | TyVar i, TyVar j -> if i = j then Some (TyVar i) else None
  | Arr(tys1, tyt1), Arr(tys2, tyt2) ->
     (match join ctx tys1 tys2, join ctx tyt1 tyt2 with
        Some tys, Some tyt -> Some (Arr(tys, tyt))
      | _ -> None)
  | Forall(id, ty1'), Forall(_, ty2') ->
     (match join ((id,STVar)::ctx) ty1' ty2' with
        Some ty -> Some ty
      | None -> join ctx (typeInst ty1' Dyn) (typeInst ty2' Dyn)) (* Correct? *)
  | Forall(id, ty1'), _ when containsDyn ty2 ->
     join ((id,GTVar)::ctx) ty1' (typeShift 1 0 ty2)
  | _, Forall(id, ty2') when containsDyn ty1 ->
     join ((id,GTVar)::ctx) (typeShift 1 0 ty1) ty2'
  | List ty10, List ty20 ->
     (match join ctx ty10 ty20 with
        Some ty -> Some (List ty)
      | None -> None)
  | _ -> if forall (isGradual ctx) (freeTVs ty1)
            && forall (isGradual ctx) (freeTVs ty2)
         then Some Dyn else None

let rec meet ctx ty1 ty2 =
  match ty1, ty2 with
    Dyn, _ -> if forall (isGradual ctx) (freeTVs ty2) then Some ty2 else None
  | _, Dyn -> if forall (isGradual ctx) (freeTVs ty1) then Some ty1 else None
  | Int, Int -> Some Int
  | Bool, Bool -> Some Bool
  | TyVar i, TyVar j -> if i = j then Some (TyVar i) else None
  | Arr(tys1, tyt1), Arr(tys2, tyt2) ->
     (match meet ctx tys1 tys2, meet ctx tyt1 tyt2 with
        Some tys, Some tyt -> Some (Arr(tys, tyt))
      | _ -> None)
  | Forall(id, ty1'), Forall(_, ty2') -> meet ((id,STVar)::ctx) ty1' ty2'
  (* not sure if the following two clauses are correct *)
  | Forall(id, ty1'), _ -> meet ((id,GTVar)::ctx) (typeShift 1 0 ty1') ty2
  | _, Forall(id, ty2') -> meet ((id,GTVar)::ctx) ty1 (typeShift 1 0 ty2')
  | List ty10, List ty20 ->
     (match meet ctx ty10 ty20 with
        Some ty -> Some (List ty)
      | None -> None)
  | _, _ -> None
                              
let typeOfBin = function
    (Plus | Mult) -> Int, Int, Int
  | (Lt | Eq) -> Int, Int, Bool

type tyenv = (id * binding) list
exception TypeError of
            Lexing.position
            * ((Format.formatter -> ty -> unit) -> ty -> unit, Format.formatter, unit) Pervasives.format
            * tyenv * ty
exception TypeError2 of
            Lexing.position
            * ((Format.formatter -> ty -> unit) -> ty -> (Format.formatter -> ty -> unit) -> ty -> unit, Format.formatter, unit) Pervasives.format
            * tyenv * ty * ty

module FC =
  struct
    open Syntax.FC

    let con ctx ty1 ty2 = precon isGradual ctx ty1 ty2

    let rec typeOf ctx = function
        Var(r,i) ->
        (match List.nth ctx i with
           (_, VDecl ty) -> typeShift (i+1) 0 ty
         | (id, _) -> errAt r.frm (Format.sprintf "var: %s is not a term variable!?" id))
      | IConst(_,_) -> Int
      | BConst(_,_) -> Bool
      | BinOp(r, op, e1, e2) ->
          let ty1, ty2 = typeOf ctx e1, typeOf ctx e2 in
          let ty1', ty2', ty3 = typeOfBin op in
          if ty1 = ty1' then
            if ty2 = ty2' then ty3
            else raise (TypeError2 (tmPos e2, "binop: the second arg has type %a but is expected to have type %a", ctx, ty2, ty2'))
          else raise (TypeError2 (tmPos e1, "binop: the first arg has type %a but is expected to have type %a", ctx, ty1, ty1'))
      | IfExp(r, e1, e2, e3) ->
         (match typeOf ctx e1 with
            Bool -> let ty2 = typeOf ctx e2 in
                    let ty3 = typeOf ctx e3 in
                    if ty2 = ty3 then ty2
                    else raise (TypeError2 (r.frm, "if: types of branches %a and %a do not match", ctx, ty2, ty3))
          | ty1 -> raise (TypeError (tmPos e1, "if: the test exp has type %a but is expected to have type Bool", ctx, ty1)))
      | FunExp(r, id, ty, e0) ->
         let tybody = typeOf ((id, VDecl ty)::ctx) e0 in
         Arr(ty, typeShift (-1) 0 tybody)
      | FixExp(r, id1, id2, ty1, ty2, e0) ->
         let tyfun = Arr(ty1, ty2) in
         let ty1' = typeShift 1 0 ty1 in
         let tybody = typeShift (-2) 0  (* there are two bound variables *)
                                (typeOf ((id2, VDecl ty1')::(id1, VDecl tyfun)::ctx) e0) in
         if tybody = ty2 then tyfun
         else raise (TypeError2 (r.frm, "fix: the body has type %a but is expected to have type %a", ctx, tybody, ty2))
      | AppExp(r, e1, e2) ->
         let ty1 = typeOf ctx e1 in
         let ty2 = typeOf ctx e2 in
         (match ty1 with
          | Arr(ty11, ty12) when ty11 = ty2 -> ty12
          | Arr(ty11, _) ->
             raise (TypeError2 (r.frm, "app: the argument has type %a but is expected to be consistent with %a", ctx, ty2, ty11))
          | _ -> raise (TypeError (tmPos e1, "app: the expression has type %a but is expected to have ->", ctx, ty1)))
      | TSFunExp(r, id, e0) ->
         Forall(id, typeOf ((id,STVar)::ctx) e0)
      | TGFunExp(r, id, e0) ->
         Forall(id, typeOf ((id,GTVar)::ctx) e0)
      | TAppExp(r, e1, ty) ->
         let ty1 = typeOf ctx e1 in
         (match ty1 with
          | Forall(id, ty11) -> typeInst ty11 ty
          | _ -> raise (TypeError (tmPos e1, "app: the expression has type %a but is expected to have All", ctx, ty1)))
      | CastExp(r, e0, ty1, ty2) ->
         let ty0 = typeOf ctx e0 in
         if ty0 = ty1 then
           if con ctx ty1 ty2 then ty2
           else raise (TypeError2 (r.frm, "cast: the source type %a and target type %a are not consistent", ctx, ty1, ty2))
         else raise (TypeError2 (tmPos e0, "cast: the expression has type %a but is expected to have %a", ctx, ty0, ty1))
      | NilExp(r, ty) -> List ty
      | ConsExp(r, e1, e2) ->
         let ty1 = typeOf ctx e1 in
         let ty2 = typeOf ctx e2 in
         (match ty2 with
          | List ty0 -> if ty1 = ty0 then ty2
                        else raise (TypeError2 (tmPos e1, "cons: the expression has type %a but is expected to have %a", ctx, ty1, ty0))
          | _ -> raise (TypeError (tmPos e2, "cons: the expression has type %a but is expected to have list or *", ctx, ty2)))
      | MatchExp(r, e0, e1, x, y, e2) ->
         (match typeOf ctx e0 with
            List ty0 -> let ty1 = typeOf ctx e1 in
                        let ty2 = typeShift (-2) 0 (typeOf ((y, VDecl (List (typeShift 1 0 ty0))) :: (x, VDecl ty0) :: ctx) e2) in
                        if ty1 = ty2 then ty1
                        else raise (TypeError2 (r.frm, "match: types of branches %a and %a do not match", ctx, ty1, ty2))
          | ty0 -> raise (TypeError (tmPos e0, "if: the test exp has type %a but is expected to have a list type", ctx, ty0)))

    let typingDecl ctx = function
        Prog e | Decl(_, e) -> typeOf ctx e
  end

 module FG =
   struct
     open Syntax
     open Syntax.FG
     let freeze tenv = List.map
                         (function
                            (id, GTVar, {contents=false}) -> (id, STVar)
                          | (id, k, _) -> (id, k))
                         tenv

     let isGradualOrMakeItGradual ctx i =
       match List.nth ctx i with
         (_, STVar) -> false
       | (_, GTVar) -> true
       | (_, PossiblySTVar f) -> f := false; true
       | (id, _) -> err ("isGradual: cannot happen -- " ^ id)

     let rec con ctx ty1 ty2 = precon isGradualOrMakeItGradual ctx ty1 ty2

     let matchingFun ctx f1 = function
         Dyn -> FC.CastExp(FC.tmRan f1, f1, Dyn, Arr(Dyn, Dyn)), Dyn, Dyn
       | Arr(dom,cod) -> f1, dom, cod
       | ty -> raise (TypeError (FC.tmPos f1, "The expression has type %a but is expected to have -> or *", ctx, ty))

     let matchingTFun ctx f1 = function
         Dyn ->
          let ty = Forall("dummy", Dyn) in
          FC.CastExp(FC.tmRan f1, f1, Dyn, ty), Dyn
       | Forall(_,ty11) -> f1, ty11
       | ty -> raise (TypeError (FC.tmPos f1, "The expression has type %a but is expected to have All or *", ctx, ty))

     let matchingList ctx f1 = function
         Dyn -> FC.CastExp(FC.tmRan f1, f1, Dyn, List Dyn), Dyn
       | List ty -> f1, ty
       | ty -> raise (TypeError (FC.tmPos f1, "The expression has type %a but is expected to have list or *", ctx, ty))

     let putOpCast f ?(ran=FC.tmRan f) ctx src tgt =
       if src = tgt then f
       else FC.CastExp(ran, f, src, tgt)

     let rec translate ctx = function
         Var(r,i) ->
         (match List.nth ctx i with
            (_, VDecl ty) -> FC.Var(r, i), typeShift (i+1) 0 ty
          | (id, _) -> errAt r.frm (Format.sprintf "var: %s is not a term variable!?" id))
       | IConst(r,i) -> FC.IConst(r,i), Int
       | BConst(r,b) -> FC.BConst(r,b), Bool
       | BinOp(r, op, e1, e2) ->
          let (f1, ty1), (f2, ty2) = translate ctx e1, translate ctx e2 in
          let ty1', ty2', ty3 = typeOfBin op in
          if con ctx ty1 ty1' then
            if con ctx ty2 ty2' then
              FC.BinOp(r, op, putOpCast f1 ctx ty1 ty1', putOpCast f2 ctx ty2 ty2'),
              ty3
            else raise (TypeError2 (tmPos e2, "binop: the second arg has type %a but is expected to have type %a", ctx, ty2, ty2'))
          else raise (TypeError2 (tmPos e1, "binop: the first arg has type %a but is expected to have type %a", ctx, ty1, ty1'))
       | IfExp(r, e1, e2, e3) ->
          let f1, ty1 = translate ctx e1 in
          (match ty1 with
             Bool | Dyn ->
                     let (f2, ty2) = translate ctx e2 in
                     let (f3, ty3) = translate ctx e3 in
                     (match meet ctx ty2 ty3 with
                        Some ty -> FC.IfExp(r, putOpCast f1 ctx ty1 Bool,
                                            putOpCast f2 ctx ty2 ty,
                                            putOpCast f3 ctx ty3 ty),
                                   ty
                      | None -> raise (TypeError2 (r.frm, "if: types of branches %a and %a do not match (no meet)", ctx, ty2, ty3)))
           | ty1 -> raise (TypeError (tmPos e1, "if: the test exp has type %a but is expected to have type Bool or *", ctx, ty1)))
       | FunExp(r, id, ty, e0) ->
          let f0, tybody = translate ((id, VDecl ty)::ctx) e0 in
          FC.FunExp(r, id, ty, f0), Arr(ty, typeShift (-1) 0 tybody)
       | FixExp(r, id1, id2, ty1, ty2, e0) ->
          let tyfun = Arr(ty1, ty2) in
          let ty1' = typeShift 1 0 ty1 in
          let f0, tybody = translate ((id2, VDecl ty1')::(id1, VDecl tyfun)::ctx) e0 in
          let tybody = typeShift (-2) 0 tybody in  (* there are two bound variables *)
          if tybody = ty2 then FC.FixExp(r, id1, id2, ty1, ty2, f0), tyfun (* tybody = ty2 ?? *)
          else raise (TypeError2 (r.frm, "fix: the body has type %a but is expected to have type %a", ctx, tybody, ty2))
       | AppExp(r, e1, e2) ->
          let f1, ty1 = translate ctx e1 in
          let f1, ty11, ty12 = matchingFun ctx f1 ty1 in
          let f2, ty2 = translate ctx e2 in
          if ty11 = ty2 then FC.AppExp(r, f1, f2), ty12
          else if con ctx ty11 ty2 then
            FC.AppExp(r, f1, FC.CastExp(FC.tmRan f2, f2, ty2, ty11)), ty12
          else raise (TypeError2 (r.frm, "app: the argument has type %a but is expected to be consistent with %a", ctx, ty2, ty11))
       | TFunExp(r, id, e0) ->
          let flag = ref true in
          let f0, ty0 = translate ((id,PossiblySTVar flag)::ctx) e0 in
          (if !flag then FC.TSFunExp(r, id, f0) else FC.TGFunExp(r, id, f0)),
          Forall(id, ty0)
       | TAppExp(r, e1, ty) ->
          let f1, ty1 = translate ctx e1 in
          let f1', ty1' = matchingTFun ctx f1 ty1 in
          FC.TAppExp(r, f1', ty), typeInst ty1' ty
       | LetExp(r, id, e1, e2) ->
          let f1, ty1 = translate ctx e1 in
          let f2, ty2 = translate ((id, VDecl ty1)::ctx) e2 in
          FC.AppExp(r, FC.FunExp(Support.Error.dummy_range,id,ty1,f2), f1), typeShift (-1) 0 ty2
       | AscExp(r, e1, ty) ->
          let f1, ty1 = translate ctx e1 in
          if con ctx ty1 ty then putOpCast f1 ~ran:r ctx ty1 ty, ty
          else raise (TypeError2 ((tmRan e1).frm, "Ascription: the expression has type %a but is not consistent with %a", ctx, ty1, ty))
       | CastExp(r, e0, ty1, ty2) ->
          let f1, ty1' = translate ctx e0 in
          if ty1 = ty1' then
            if con ctx ty1 ty2 then FC.CastExp(r, f1, ty1, ty2), ty2
            else raise (TypeError2 (r.frm, "cast: the source type %a and target type %a are not consistent", ctx, ty1, ty2))
          else raise (TypeError2 (tmPos e0, "cast: the expression has type %a but is expected to have %a", ctx, ty1', ty1))
      | NilExp(r, ty) -> FC.NilExp(r, ty), List ty
      | ConsExp(r, e1, e2) ->
         let f1, ty1 = translate ctx e1 in
         let f2, ty2 = translate ctx e2 in
         let f2', ty20 = matchingList ctx f2 ty2 in
         if con ctx ty1 ty20 then
           FC.ConsExp(r, putOpCast f1 ctx ty1 ty20, f2'), List ty20
         else raise (TypeError2 (tmPos e1, "cons: the expression has type %a but is expected to have %a", ctx, ty1, ty20))
      | MatchExp(r, e0, e1, x, y, e2) ->
         let f0, ty0 = translate ctx e0 in
         let f0', ty00 = matchingList ctx f0 ty0 in
         let f1, ty1 = translate ctx e1 in
         let f2, ty2 = translate ((y, VDecl (List (typeShift 1 0 ty00)))
                                  :: (x, VDecl ty00) :: ctx) e2 in
         let ty2 = typeShift (-2) 0 ty2 in
         (match meet ctx ty1 ty2 with
            Some ty -> FC.MatchExp(r, f0',
                                   putOpCast f1 ctx ty1 ty, x, y,
                                   putOpCast f2 ctx ty2 ty), ty
          | None -> raise (TypeError2 (r.frm, "match: types of branches %a and %a do not match", ctx, ty1, ty2)))

     let translateDecl ctx = function
         Prog e ->
          let f, tye = translate ctx e in
          FC.Prog f, tye
       | Decl(id, e) ->
          let f, tye = translate ctx e in
          FC.Decl(id, f), tye
   end
