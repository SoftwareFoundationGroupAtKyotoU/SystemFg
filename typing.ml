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

let rec forall p = function
    [] -> true
  | x::l -> p x && forall p l

let rec freeTVs i = function
    Int -> []
  | Bool -> []
  | Arr(ty1, ty2) -> merge (freeTVs i ty1) (freeTVs i ty2)
  | TyVar j -> if j > i then [j] else []
  | Forall(_, ty0) -> freeTVs (i+1) ty0
  | Dyn -> []

let freeTVs = freeTVs 0

let rec containsDyn = function
    Int -> false
  | Bool -> false
  | Arr(ty1, ty2) -> containsDyn ty1 || containsDyn ty2
  | TyVar _ -> false
  | Forall(_, ty0) -> containsDyn ty0
  | Dyn -> true

let rec con ctx ty1 ty2 =
  ty1 = ty2 ||
    match ty1, ty2 with
      Arr(tys1, tyt1), Arr(tys2, tyt2) ->
        con ctx tys1 tys2 && con ctx tyt1 tyt2
    | Forall(id, ty1'), Forall(_, ty2') ->
       con ((id,STVar)::ctx) ty1' ty2'
    | Forall(id, ty1'), _ ->
       containsDyn ty2 && con ((id,GTVar)::ctx) ty1' (typeShift 1 0 ty2)
    | _, Forall(id, ty2') ->
       containsDyn ty1 && con ((id,GTVar)::ctx) (typeShift 1 0 ty1) ty2'
    | Dyn, _ -> forall (isGradual ctx) (freeTVs ty2)
    | _, Dyn -> forall (isGradual ctx) (freeTVs ty1)
    | _ -> false

let rec join ctx ty1 ty2 =
  match ty1, ty2 with
    Dyn, _ -> if forall (isGradual ctx) (freeTVs ty2) then Some Dyn else None
  | _, Dyn -> if forall (isGradual ctx) (freeTVs ty1) then Some Dyn else None
  | Int, Int -> Some Int
  | Bool, Bool -> Some Bool
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
  | _ -> if forall (isGradual ctx) (freeTVs ty1)
            && forall (isGradual ctx) (freeTVs ty2)
         then Some Dyn else None

let typeOfBin = function
    (Plus | Mult) -> Int, Int, Int
  | Lt -> Int, Int, Bool

module FC =
  struct
    open Syntax.FC
    let rec typeOf ctx = function
        Var(r,i) ->
        (match List.nth ctx i with
           (_, VDecl ty) -> typeShift (i+1) 0 ty
         | (id, _) -> errAt r.frm ("var: "^id^"is not a term variable!?"))
      | IConst(_,_) -> Int
      | BConst(_,_) -> Bool
      | BinOp(r, op, e1, e2) ->
          let ty1, ty2 = typeOf ctx e1, typeOf ctx e2 in
          let ty1', ty2', ty3 = typeOfBin op in
          if ty1 = ty1' then
            if ty2 = ty2' then ty3
            else errAt (tmPos e2) ("binop: second arg not as expected")
          else errAt (tmPos e1) ("binop: first arg not as expected")
      | IfExp(r, e1, e2, e3) ->
         (match typeOf ctx e1 with
            Bool -> let ty2 = typeOf ctx e2 in
                    if ty2 = typeOf ctx e3 then ty2
                    else errAt r.frm "if: types of branches do not match"
          | _ -> errAt (tmPos e1) "if: type of test not of Bool")
      | FunExp(r, id, ty, e0) ->
         let tybody = typeOf ((id, VDecl ty)::ctx) e0 in
         Arr(ty, typeShift (-1) 0 tybody)
      | AppExp(r, e1, e2) ->
         let ty1 = typeOf ctx e1 in
         let ty2 = typeOf ctx e2 in
         (match ty1 with
          | Arr(ty11, ty12) when con ctx ty11 ty2 -> ty12
          | Arr(ty11, _) ->
             errAt r.frm "app: the type of arg isn't consistent with the expected argument type"
          | _ -> errAt (tmPos e1) "app: the type of the expression isn't -> or *")
      | TSFunExp(r, id, e0) ->
         Forall(id, typeOf ((id,STVar)::ctx) e0)
      | TGFunExp(r, id, e0) ->
         Forall(id, typeOf ((id,GTVar)::ctx) e0)
      | TAppExp(r, e1, ty) ->
         let ty1 = typeOf ctx e1 in
         (match ty1 with
          | Forall(id, ty11) -> typeInst ty11 ty
          | _ -> errAt (tmPos e1) "tyapp: not a polymorphic function")
      | CastExp(r, e0, ty1, ty2) ->
         let ty0 = typeOf ctx e0 in
         if ty0 = ty1 then
           if con ctx ty1 ty2 then ty2
           else errAt r.frm "cast: the source and target types are not consistent"
         else errAt (tmPos e0) "cast: the type of exp is not the given source type"

    let typingDecl ctx = function
        Prog e -> typeOf ctx e
      | Decl(_, ty, e) ->
         let tye = typeOf ctx e in
         if ty = tye then ty
         else errAt (tmPos e) "let: the type of exp isn't as declared"
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

     let rec con ctx ty1 ty2 =
       ty1 = ty2 ||
         match ty1, ty2 with
           Arr(tys1, tyt1), Arr(tys2, tyt2) ->
            con ctx tys1 tys2 && con ctx tyt1 tyt2
         | Forall(id, ty1'), Forall(_, ty2') ->
            con ((id,STVar)::ctx) ty1' ty2'
         | Forall(id, ty1'), _ ->
            containsDyn ty2 && con ((id,GTVar)::ctx) ty1' (typeShift 1 0 ty2)
         | _, Forall(id, ty2') ->
            containsDyn ty1 && con ((id,GTVar)::ctx) (typeShift 1 0 ty1) ty2'
         | Dyn, _ -> forall (isGradualOrMakeItGradual ctx) (freeTVs ty2)
         | _, Dyn -> forall (isGradualOrMakeItGradual ctx) (freeTVs ty1)
         | _ -> false

     let matchingFun ctx f1 = function
         Dyn -> FC.CastExp(FC.tmRan f1, f1, Dyn, Arr(Dyn, Dyn)), Dyn, Dyn
       | Arr(dom,cod) -> f1, dom, cod
       | ty -> errAt (FC.tmPos f1) (Printf.sprintf "Type %s does not match with -> or *" (string_of_ty ctx ty))

     let matchingTFun f1 = function
         Dyn ->
          let ty = Forall("dummy", Dyn) in
          FC.CastExp(FC.tmRan f1, f1, Dyn, ty), Dyn
       | Forall(_,ty11) -> f1, ty11
       | _ -> errAt (FC.tmPos f1) ("Type does not match with forall or *")

     let putOpCast ctx src tgt f =
       if src = tgt then f
       else FC.CastExp(FC.tmRan f, f, src, tgt)

     let rec translate ctx = function
         Var(r,i) ->
         (match List.nth ctx i with
            (_, VDecl ty) -> FC.Var(r, i), typeShift (i+1) 0 ty
          | (id, _) -> errAt r.frm ("var: "^id^"is not a term variable!?"))
       | IConst(r,i) -> FC.IConst(r,i), Int
       | BConst(r,b) -> FC.BConst(r,b), Bool
       | BinOp(r, op, e1, e2) ->
          let (f1, ty1), (f2, ty2) = translate ctx e1, translate ctx e2 in
          let ty1', ty2', ty3 = typeOfBin op in
          if con ctx ty1 ty1' then
            if con ctx ty2 ty2' then
              FC.BinOp(r, op, putOpCast ctx ty1 ty1' f1, putOpCast ctx ty2 ty2' f2),
              ty3
            else errAt (tmPos e2) ("binop: second arg not as expected")
          else errAt (tmPos e1) ("binop: first arg not as expected")
       | IfExp(r, e1, e2, e3) ->
          let f1, t1 = translate ctx e1 in
          (match t1 with
             Bool -> let (f2, ty2) = translate ctx e2 in
                     let (f3, ty3) = translate ctx e3 in
                     (match join ctx ty2 ty3 with
                        Some ty -> FC.IfExp(r, f1,
                                            putOpCast ctx ty2 ty f2,
                                            putOpCast ctx ty3 ty f3),
                                   ty
                      | None -> errAt r.frm "if: types of branches do not match (there is no join)")
           | _ -> errAt (tmPos e1) "if: type of test not of Bool")
       | FunExp(r, id, ty, e0) ->
          let f0, tybody = translate ((id, VDecl ty)::ctx) e0 in
          FC.FunExp(r, id, ty, f0), Arr(ty, typeShift (-1) 0 tybody)
       | AppExp(r, e1, e2) ->
          let f1, ty1 = translate ctx e1 in
          let f1, ty11, ty12 = matchingFun ctx f1 ty1 in
          let f2, ty2 = translate ctx e2 in
          if ty11 = ty2 then FC.AppExp(r, f1, f2), ty12
          else if con ctx ty11 ty2 then
            FC.AppExp(r, f1, FC.CastExp(FC.tmRan f2, f2, ty2, ty11)), ty12
          else errAt r.frm "app: the type of arg isn't consistent with the expected argument type"
       | TFunExp(r, id, e0) ->
          let flag = ref true in
          let f0, ty0 = translate ((id,PossiblySTVar flag)::ctx) e0 in
          (if !flag then FC.TSFunExp(r, id, f0) else FC.TGFunExp(r, id, f0)),
          Forall(id, ty0)
       | TAppExp(r, e1, ty) ->
          let f1, ty1 = translate ctx e1 in
          let f1', ty1' = matchingTFun f1 ty1 in
          FC.TAppExp(r, f1', ty), typeInst ty1' ty
       | AscExp(r, e1, ty) ->
          let f1, ty1 = translate ctx e1 in
          if con ctx ty1 ty then FC.CastExp(r, f1, ty1, ty), ty
          else errAt r.frm "Ascription: not compatible"
       | CastExp(r, e1, ty1, ty2) ->
          let f1, ty1' = translate ctx e1 in
          if ty1 = ty1' then
            if con ctx ty1 ty2 then FC.CastExp(r, f1, ty1, ty2), ty2
            else errAt r.frm "Cast: src and target not compatible"
          else errAt (tmPos e1) "Cast: doesn't match the src type"

     let translateDecl ctx = function
         Prog e ->
         let f, tye = translate ctx e in
         FC.Prog f, tye
       | Decl(id, ty, e) ->
          let f, tye = translate ctx e in
          if con ctx tye ty then
            FC.Decl (id, ty, putOpCast ctx tye ty f), ty
          else errAt (tmPos e) "let: the type of exp isn't as declared"
   end
