%{
open Support.Error
open Syntax
open FG
%}

%token <Support.Error.range> LPAREN RPAREN LBRACKET RBRACKET SEMISEMI
%token <Support.Error.range> RARROW DARROW COLON DOT
%token <Support.Error.range> PLUS AST LT EQ
%token <Support.Error.range> IF THEN ELSE TRUE FALSE LET IN FUN REC INT BOOL ALL

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
      let t, ty = plist ctx e ty in
      Decl (id.v, ty, t) }
/*
  | LET REC ID EQ FUN ID RARROW Expr SEMISEMI { RecDecl ($3, $6, $8) }
*/

expr :
    e=ifExpr { e }
  | e=funExpr { e }
  | e=letExpr { e }
/*  | letRecExpr { $1 } */
  | e=lTExpr { e }

lTExpr :
    e1=pExpr LT e2=pExpr { fun ctx ->
      let e1 = e1 ctx in
      let e2 = e2 ctx in
      BinOp (join_range_exps e1 e2, Lt, e1, e2)
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

ifExpr :
    start=IF e1=expr THEN e2=expr ELSE e3=expr { fun ctx ->
      let e3 = e3 ctx in
      IfExp (join_range start (tmRan e3), e1 ctx, e2 ctx, e3)
    }

letExpr :
    start=LET id=LCID plist=letParamList COLON ty=ty EQ e=expr IN body=expr { fun ctx ->
      let (t, ty) = plist ctx e ty in
      let body = body ((id.v,VDecl ty)::ctx) in
      AppExp(join_range start (tmRan body),
             FunExp(Support.Error.dummy_range, id.v, ty, body), t)
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
      let body = t ((id.v,VDecl ty)::ctx) in
      FunExp(join_range start (tmRan body), id.v, ty, body)
    }
  | id=UCID { fun ctx t ->
      let body = t ((id.v,PossiblySTVar (ref true))::ctx) in
      TFunExp(join_range id.r (tmRan body), id.v, body)
    }
  | start=LPAREN id=LCID COLON ty=ty RPAREN rest=funParamList { fun ctx t ->
      let ty = ty ctx in
      let body = rest ((id.v,VDecl ty)::ctx) t in
      FunExp(join_range start (tmRan body), id.v, ty, body)
    }
  | id=UCID rest=funParamList { fun ctx t ->
      let body = rest ((id.v,PossiblySTVar (ref true))::ctx) t in
      TFunExp(join_range id.r (tmRan body), id.v, body)
    }

letParamList :
    /* empty */ { fun ctx t ty -> (t ctx, ty ctx) }
  | start=LPAREN id=LCID COLON paramty=ty RPAREN rest=letParamList { fun ctx t ty ->
       let paramty = paramty ctx in
       let (t, ty) = rest ((id.v, VDecl paramty)::ctx) t ty in
       FunExp(join_range start (tmRan t), id.v, paramty, t), Arr(paramty, typeShift (-1) 0 ty)
    }
  | id=UCID rest=letParamList { fun ctx t ty ->
       let (t', ty') = rest ((id.v,PossiblySTVar (ref true))::ctx) t ty in
       TFunExp(join_range id.r (tmRan t'), id.v, t'), Forall(id.v, ty')
    }

ty :
  | ty1=aType RARROW ty2=ty { fun ctx -> Arr(ty1 ctx, ty2 ctx) }
  | ALL id=neTVSeq DOT ty=ty { fun ctx -> id ctx ty }
  | ty=aType { ty }

neTVSeq : /* nonempty type var sequence */
    id=PRIMEUCID { fun ctx ty -> Forall(id.v, ty ((id.v,STVar)::ctx)) }
  | id=UCID { fun ctx ty -> Forall(id.v, ty ((id.v,GTVar)::ctx)) }
  | id=PRIMEUCID rest=neTVSeq { fun ctx ty -> Forall(id.v, rest ((id.v,STVar)::ctx) ty) }
  | id=UCID rest=neTVSeq { fun ctx ty -> Forall(id.v, rest ((id.v,GTVar)::ctx) ty) }

aType :
    INT { fun ctx -> Int }
  | BOOL { fun ctx -> Bool }
  | AST { fun ctx -> Dyn }
  | id=UCID { fun ctx -> TyVar (name2index ctx id) }
  | id=PRIMEUCID { fun ctx -> TyVar (name2index ctx id) }
  | LPAREN ty=ty RPAREN { ty }
