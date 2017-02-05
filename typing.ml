open Syntax

let rec isStatic ctx i =
  match List.nth ctx i with
    (_, STVar) -> true
  | (_, GTVar) -> false
  | (id, _) -> failwith ("isStatic: cannot happen -- " ^ id)

let rec isGradual ctx i =
  match List.nth ctx i with
    (_, STVar) -> false
  | (_, GTVar) -> true
  | (id, _) -> failwith ("isStatic: cannot happen -- " ^ id)

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
      Arr(tys1, tyt1), Arr(tys2, tyt2) -> con ctx tys1 tys2 && con ctx tyt1 tyt2
    | Forall(id, ty1'), Forall(_, ty2') -> con ((id,STVar)::ctx) ty1' ty2'
    | Forall(id, ty1'), _ -> containsDyn ty2 && con ((id,GTVar)::ctx) ty1' (typeShift 1 0 ty2)
    | _, Forall(id, ty2') -> containsDyn ty1 && con ((id,GTVar)::ctx) (typeShift 1 0 ty1) ty2'
    | Dyn, _ -> forall (isGradual ctx) (freeTVs ty2)
    | _, Dyn -> forall (isGradual ctx) (freeTVs ty1)
    | _ -> false

let rec typeOf ctx = function
    Var i ->
    (match List.nth ctx i with
       (_, VDecl ty) -> ty
     | (id, _) -> failwith ("var: "^id^"is not a term variable!?"))
  | IConst _ -> Int
  | BConst _ -> Bool
  | BinOp((Plus | Mult), e1, e2) ->
     (match typeOf ctx e1, typeOf ctx e2 with
        Int, Int -> Int
      | Int, _ -> failwith ("binop: second arg not of Int")
      | _ ->  failwith ("binop: first arg not of Int"))
  | BinOp(Lt, e1, e2) ->
     (match typeOf ctx e1, typeOf ctx e2 with
        Int, Int -> Bool
      | Int, _ -> failwith ("binop: second arg not of Int")
      | _ ->  failwith ("binop: first arg not of Int"))
  | IfExp(e1, e2, e3) ->
     (match typeOf ctx e1 with
        Bool -> let ty2 = typeOf ctx e2 in
                if ty2 = typeOf ctx e3 then ty2
                else failwith "if: types of branches do not match"
      | _ -> failwith "if: type of test not of Bool")
  | FunExp(id, ty, e0) ->
     Arr(ty, typeOf ((id, VDecl ty)::ctx) e0)
  | AppExp(e1, e2) ->
     let ty1 = typeOf ctx e1 in
     let ty2 = typeOf ctx e2 in
     (match ty1 with
      | Arr(ty11, ty12) when con ctx ty11 ty2 -> ty12
      | Arr(ty11, _) ->
         failwith "app: the type of arg isn't consistent with the expected argument type"
      | _ -> failwith "app: the type of the expression isn't -> or *")
  | TSFunExp(id, e0) ->
     Forall(id, typeOf ((id,STVar)::ctx) e0)
  | TGFunExp(id, e0) ->
     Forall(id, typeOf ((id,GTVar)::ctx) e0)
  | TAppExp(e1, ty) ->
     let ty1 = typeOf ctx e1 in
     (match ty1 with
      | Forall(id, ty11) -> typeInst ty11 ty
      | _ -> failwith "tyapp: not a polymorphic function")
  | CastExp(e0, ty1, ty2) ->
     let ty0 = typeOf ctx e0 in
     if ty0 = ty1 then
       if con ctx ty1 ty2 then ty2
       else failwith "cast: the source and target types are not consistent"
     else failwith "cast: the type of exp is not the given source type"

let typingDecl ctx = function
    Prog e -> typeOf ctx e
  | Decl(_, ty, e) ->
     let tye = typeOf ctx e in
     if ty = tye then ty
     else failwith "let: the type of exp isn't as declared"
