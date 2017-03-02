%{
open Support.Error
open Syntax
open FG
%}

%token <Lexing.position> LPAREN RPAREN LBRACKET RBRACKET SEMISEMI
%token <Lexing.position> RARROW DARROW COLON DOT
%token <Lexing.position> PLUS AST LT EQ
%token <Lexing.position> IF THEN ELSE TRUE FALSE LET IN FUN REC INT BOOL ALL

%token <int Support.Error.with_pos> INTV
%token <Syntax.id Support.Error.with_pos> LCID
%token <Syntax.id Support.Error.with_pos> PRIMEUCID
%token <Syntax.id Support.Error.with_pos> UCID

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
      BinOp (tmPos e1, Lt, e1, e2)
    }
  | e=pExpr { e }

pExpr :
    e1=pExpr PLUS e2=mExpr { fun ctx ->
      let e1 = e1 ctx in
      let e2 = e2 ctx in
      BinOp (tmPos e1, Plus, e1, e2) }
  | e=mExpr { e }

mExpr :
    e1=mExpr AST e2=appExpr { fun ctx ->
      let e1 = e1 ctx in
      let e2 = e2 ctx in
      BinOp (tmPos e1, Mult, e1, e2) }
  | e=appExpr { e }

appExpr :
    e1=appExpr e2=aExpr { fun ctx ->
      let e1 = e1 ctx in
      let e2 = e2 ctx in
      AppExp (tmPos e1, e1, e2) }
  | e1=appExpr LBRACKET ty=ty RBRACKET { fun ctx ->
      let e1 = e1 ctx in
      TAppExp (tmPos e1, e1, ty ctx) }
  | e=aExpr { e }

aExpr :
    i=INTV { fun ctx -> IConst(i.p, i.v) }
  | pos=TRUE { fun ctx -> BConst(pos, true) }
  | pos=FALSE { fun ctx -> BConst(pos, false) }
  | id=LCID { fun ctx -> Var (id.p, name2index ctx id) }
  | LPAREN e=expr RPAREN { e }
  | pos=LPAREN e=expr COLON tgt=ty RPAREN
      { fun ctx -> AscExp(pos, e ctx, tgt ctx) }
  | pos=LPAREN e=expr COLON src=ty DARROW tgt=ty RPAREN
      { fun ctx -> CastExp(pos, e ctx, src ctx, tgt ctx) }

ifExpr :
    pos=IF e1=expr THEN e2=expr ELSE e3=expr
      { fun ctx -> IfExp (pos, e1 ctx, e2 ctx, e3 ctx) }

letExpr :
    pos=LET id=LCID plist=letParamList COLON ty=ty EQ e=expr IN body=expr { fun ctx ->
      let (t, ty) = plist ctx e ty in
      AppExp(pos, FunExp(Lexing.dummy_pos, id.v, ty, body ((id.v,VDecl ty)::ctx)), t)
    }

funExpr :
    FUN plist=funParamList RARROW body=expr { fun ctx -> plist ctx body }

/*
LetRecExpr :
    LET REC LCID EQ FUN LCID RARROW Expr IN Expr { LetRecExp ($3, $6, $8, $10) }
*/

funParamList :
    pos=LPAREN id=LCID COLON ty=ty RPAREN { fun ctx t ->
      let ty = ty ctx in
      FunExp(pos, id.v, ty, t ((id.v,VDecl ty)::ctx)) }
  | id=UCID { fun ctx t ->
      TFunExp(id.p, id.v, t ((id.v,PossiblySTVar (ref true))::ctx)) }
  | pos=LPAREN id=LCID COLON ty=ty RPAREN rest=funParamList { fun ctx t ->
      let ty = ty ctx in
      FunExp(id.p, id.v, ty, rest ((id.v,VDecl ty)::ctx) t) }
  | id=UCID rest=funParamList { fun ctx t ->
      TFunExp(id.p, id.v, rest ((id.v,PossiblySTVar (ref true))::ctx) t) }

letParamList :
    /* empty */ { fun ctx t ty -> (t ctx, ty ctx) }
  | pos=LPAREN id=LCID COLON ty=ty RPAREN rest=letParamList { fun ctx t ty ->
       let ty' = ty ctx in
       let (t', ty'') = rest ((id.v, VDecl ty')::ctx) t ty in
       FunExp(pos, id.v, ty', t'), Arr(ty', typeShift (-1) 0 ty'')
    }
  | id=UCID rest=letParamList { fun ctx t ty ->
       let (t', ty') = rest ((id.v,PossiblySTVar (ref true))::ctx) t ty in
       TFunExp(id.p, id.v, t'), Forall(id.v, ty')
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
