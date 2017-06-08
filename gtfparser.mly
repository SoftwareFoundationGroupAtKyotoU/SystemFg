%{
open Support.Error
open Syntax
open FG
%}

%token <Support.Error.range> LPAREN RPAREN LBRACKET RBRACKET SEMISEMI COLCOL
%token <Support.Error.range> RARROW DARROW COLON DOT AT SEMI BAR
%token <Support.Error.range> PLUS AST QM LT EQ
%token <Support.Error.range> IF THEN ELSE TRUE FALSE LET IN FUN REC MATCH WITH INT BOOL ALL LIST

%token <int Support.Error.with_ran> INTV
%token <Syntax.id Support.Error.with_ran> LCID
%token <Syntax.id Support.Error.with_ran> PRIMEUCID
%token <Syntax.id Support.Error.with_ran> UCID

%start toplevel
%type <Syntax.tyenv -> Syntax.FG.program> toplevel
%%

toplevel :
    e=expr SEMISEMI { fun ctx -> Prog (e ctx) }
  | LET id=LCID plist=letParamList COLON ty=ty EQ e=expr SEMISEMI { fun ctx ->
      let t, _ = plist ctx e (Some ty) in
      Decl (id.v, t) }
  | LET id=LCID plist=letParamList EQ e=expr SEMISEMI { fun ctx ->
      let t, _ = plist ctx e None in
      Decl (id.v, t) }
  | start=LET REC lrp=letrecParamList
                  plist=letParamList COLON ty3=ty EQ e=expr SEMISEMI { fun ctx ->
      let tplist, id1, id2, ty2 = lrp in
      let ctx' = List.fold_left (fun ctx id -> (id.v, Dummy)::ctx) ctx tplist in
      let t2, codtyop = plist ((id2.v, Dummy)::(id1.v, Dummy)::ctx) e (Some ty3) in
      match codtyop with
          Some codty ->
          let f = FixExp(join_range start (tmRan t2),id1.v,id2.v,ty2 ctx',typeShift (-2) 0 codty,t2) in
          let f' = List.fold_right
                     (fun id body ->
                       TFunExp(join_range id.r (tmRan body), id.v, body))
                     tplist f
          in Decl (id1.v, f')
        | None -> failwith "Can't happen: toplevel in gtfparser.mly"
    }


expr :
    e=ifExpr { e }
  | e=matchExpr { e }
  | e=funExpr { e }
  | e=letExpr { e }
  | e=lTExpr { e }

lTExpr :
    e1=consExpr LT e2=consExpr { fun ctx ->
      let e1 = e1 ctx in
      let e2 = e2 ctx in
      BinOp (join_range_exps e1 e2, Lt, e1, e2)
    }
  | e=consExpr { e }

consExpr :
    e1=pExpr COLCOL e2=consExpr { fun ctx ->
      let e1 = e1 ctx in
      let e2 = e2 ctx in
      ConsExp (join_range_exps e1 e2, e1, e2)
    }
  | e=pExpr { e }
      
pExpr :
    e1=pExpr PLUS e2=mExpr { fun ctx ->
      let e1 = e1 ctx in
      let e2 = e2 ctx in
      BinOp (join_range_exps e1 e2, Plus, e1, e2) }
  | e=mExpr { e }

mExpr :
    e1=mExpr AST e2=appExpr { fun ctx ->
      let e1 = e1 ctx in
      let e2 = e2 ctx in
      BinOp (join_range_exps e1 e2, Mult, e1, e2) }
  | e=appExpr { e }

appExpr :
    e1=appExpr e2=aExpr { fun ctx ->
      let e1 = e1 ctx in
      let e2 = e2 ctx in
      AppExp (join_range_exps e1 e2, e1, e2) }
  | e1=appExpr LBRACKET ty=ty last=RBRACKET { fun ctx ->
      let e1 = e1 ctx in
      TAppExp (join_range (tmRan e1) last, e1, ty ctx) }
  | e=aExpr { e }

aExpr :
    i=INTV { fun ctx -> IConst(i.r, i.v) }
  | ran=TRUE { fun ctx -> BConst(ran, true) }
  | ran=FALSE { fun ctx -> BConst(ran, false) }
  | id=LCID { fun ctx -> Var (id.r, name2index ctx id) }
  | LPAREN e=expr RPAREN { e }
  | start=LPAREN e=expr COLON tgt=ty last=RPAREN
      { fun ctx -> AscExp(join_range start last, e ctx, tgt ctx) }
  | start=LPAREN e=expr COLON src=ty DARROW tgt=ty last=RPAREN
      { fun ctx -> CastExp(join_range start last, e ctx, src ctx, tgt ctx) }
  | start=LBRACKET l=listElms last=RBRACKET { fun ctx ->
      l (join_range start last) ctx
    }

listElms :
    /* empty */ { fun r ctx -> NilExp(r, Dyn) }
  | AT ty=ty { fun r ctx -> NilExp(r, ty ctx) }
  | e=expr { fun r ctx ->
      let e = e ctx in
      ConsExp(join_range (tmRan e) r, e, NilExp(r, Dyn))
    }
  | e=expr SEMI l=listElms { fun r ctx ->
      let e = e ctx in
      let l = l r ctx in
      ConsExp(join_range (tmRan e) (tmRan l), e, l)
    }

ifExpr :
    start=IF e1=expr THEN e2=expr ELSE e3=expr { fun ctx ->
      let e3 = e3 ctx in
      IfExp (join_range start (tmRan e3), e1 ctx, e2 ctx, e3)
    }

matchExpr :
    start=MATCH e0=expr WITH
       option(BAR) LBRACKET RBRACKET RARROW e1=expr
     BAR x=LCID COLCOL y=LCID RARROW e2=expr { fun ctx ->
       let e0 = e0 ctx in
       let e1 = e1 ctx in
       let e2 = e2 ((y.v,Dummy)::(x.v,Dummy)::ctx) in
       MatchExp(join_range start (tmRan e2), e0, e1, x.v, y.v, e2)
    }

letExpr :
    start=LET id=LCID plist=letParamList COLON ty=ty EQ e=expr IN body=expr { fun ctx ->
      let t, _ = plist ctx e (Some ty) in
      let body = body ((id.v, Dummy)::ctx) in
      LetExp(join_range start (tmRan body), id.v, t, body)
    }
  | start=LET id=LCID plist=letParamList EQ e=expr IN body=expr { fun ctx ->
      let t, _ = plist ctx e None in
      let body = body ((id.v, Dummy)::ctx) in
      LetExp(join_range start (tmRan body), id.v, t, body)
    }
  | start=LET REC lrp=letrecParamList
                  plist=letParamList COLON ty3=ty EQ e=expr IN body=expr { fun ctx ->
      let tplist, id1, id2, ty2 = lrp in
      let ctx' = List.fold_left (fun ctx id -> (id.v, Dummy)::ctx) ctx tplist in
      let t2, codtyop = plist ((id2.v, Dummy)::(id1.v, Dummy)::ctx') e (Some ty3) in
      match codtyop with
          Some codty ->
          let f = FixExp(join_range start (tmRan t2),id1.v,id2.v,ty2 ctx',typeShift (-2) 0 codty,t2) in
          let f' = List.fold_right
                     (fun id body ->
                       TFunExp(join_range id.r (tmRan body), id.v, body))
                     tplist f
          in
          let body = body ((id1.v, Dummy)::ctx) in
          LetExp(join_range start (tmRan body), id1.v, f', body)
        | None -> failwith "Can't happen: letExpr in gtfparser.mly"
    }

funExpr :
    FUN plist=funParamList RARROW body=expr { fun ctx -> plist ctx body }

/*
LetRecExpr :
    LET REC LCID EQ FUN LCID RARROW Expr IN Expr { LetRecExp ($3, $6, $8, $10) }
*/

funParamList :
    start=LPAREN id=LCID COLON ty=ty RPAREN { fun ctx t ->
      let ty = ty ctx in
      let body = t ((id.v, Dummy (* VDecl ty*))::ctx) in
      FunExp(join_range start (tmRan body), id.v, ty, body)
    }
  | id=UCID { fun ctx t ->
      let body = t ((id.v, Dummy (* PossiblySTVar (ref true) *))::ctx) in
      TFunExp(join_range id.r (tmRan body), id.v, body)
    }
  | start=LPAREN id=LCID COLON ty=ty RPAREN rest=funParamList { fun ctx t ->
      let ty = ty ctx in
      let body = rest ((id.v, Dummy (* VDecl ty *))::ctx) t in
      FunExp(join_range start (tmRan body), id.v, ty, body)
    }
  | id=UCID rest=funParamList { fun ctx t ->
      let body = rest ((id.v, Dummy (* PossiblySTVar (ref true) *))::ctx) t in
      TFunExp(join_range id.r (tmRan body), id.v, body)
    }

letParamList :
    /* empty */ { fun ctx t tyop ->
       match tyop with
          Some ty -> let t, ty = t ctx, ty ctx in AscExp(tmRan t, t, ty), Some ty
           (* the source location information is a bit imprecise *)
        | None -> t ctx, None
    }
  | start=LPAREN id=LCID COLON paramty=ty RPAREN rest=letParamList { fun ctx t tyop ->
       let paramty = paramty ctx in
       let t, tyop' = rest ((id.v, Dummy (* VDecl paramty *))::ctx) t tyop in
       FunExp(join_range start (tmRan t), id.v, paramty, t),
       match tyop' with
          Some ty' -> Some (Arr(paramty, typeShift (-1) 0 ty'))
        | None -> None
    }
  | id=UCID rest=letParamList { fun ctx t tyop ->
       let t', tyop' = rest ((id.v, Dummy (* PossiblySTVar (ref true) *))::ctx) t tyop in
       TFunExp(join_range id.r (tmRan t'), id.v, t'),
       match tyop' with
          Some ty' -> Some (Forall(id.v, ty'))
        | None -> None
    }

letrecParamList : id1=LCID rest=letrecParamListRec /* f X1 ... Xn (x:T) */ { 
      let l, id2, ty2 = rest in (l, id1, id2, ty2)
    }

letrecParamListRec :  /* X1 X2 ... Xn (x:T) */
    LPAREN id2=LCID COLON ty2=ty RPAREN { ([], id2, ty2) }
  | id=UCID rest=letrecParamListRec {
      let l, id2, ty2 = rest in (id :: l, id2, ty2)
    }
                              
ty :
  | ty1=lstType RARROW ty2=ty { fun ctx -> Arr(ty1 ctx, ty2 ctx) }
  | ALL id=neTVSeq DOT ty=ty { fun ctx -> id ctx ty }
  | ty=lstType { ty }

neTVSeq : /* nonempty type var sequence */
    id=PRIMEUCID { fun ctx ty -> Forall(id.v, ty ((id.v, Dummy (* STVar *))::ctx)) }
  | id=UCID { fun ctx ty -> Forall(id.v, ty ((id.v, Dummy (* GTVar *))::ctx)) }
  | id=PRIMEUCID rest=neTVSeq { fun ctx ty -> Forall(id.v, rest ((id.v, Dummy (* STVar *))::ctx) ty) }
  | id=UCID rest=neTVSeq { fun ctx ty -> Forall(id.v, rest ((id.v, Dummy (* GTVar *))::ctx) ty) }

lstType :
  | ty0=lstType LIST { fun ctx -> List (ty0 ctx) }
  | ty=aType { ty }
  
aType :
    INT { fun ctx -> Int }
  | BOOL { fun ctx -> Bool }
  | QM { fun ctx -> Dyn }
  | id=UCID { fun ctx -> TyVar (name2index ctx id) }
  | id=PRIMEUCID { fun ctx -> TyVar (name2index ctx id) }
  | LPAREN ty=ty RPAREN { ty }
