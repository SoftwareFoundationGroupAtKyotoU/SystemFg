%{
open Syntax
open FC
%}

%token LPAREN RPAREN LBRACKET RBRACKET SEMISEMI RARROW DARROW COLON DOT
%token PLUS AST LT EQ
%token IF THEN ELSE TRUE FALSE LET IN FUN REC INT BOOL ALL

%token <int> INTV
%token <Syntax.id> LCID
%token <Syntax.id> STVarID
%token <Syntax.id> GTVarID

%start toplevel
%type <Syntax.tyenv -> Syntax.FC.program> toplevel
%%

toplevel :
    e=expr SEMISEMI { fun ctx -> Prog (e ctx) }
  | LET id=LCID plist=letParamList COLON ty=ty EQ e=expr SEMISEMI { fun ctx ->
      let t, ty = plist ctx e ty in
      Decl (id, ty, t) }
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
    e1=pExpr LT e2=pExpr { fun ctx -> BinOp (Lt, e1 ctx, e2 ctx) }
  | e=pExpr { e }

pExpr :
    e1=pExpr PLUS e2=mExpr { fun ctx -> BinOp (Plus, e1 ctx, e2 ctx) }
  | e=mExpr { e }

mExpr :
    e1=mExpr AST e2=appExpr { fun ctx -> BinOp (Mult, e1 ctx, e2 ctx) }
  | e=appExpr { e }

appExpr :
    e1=appExpr e2=aExpr { fun ctx -> AppExp (e1 ctx, e2 ctx) }
  | e1=appExpr LBRACKET ty=ty RBRACKET { fun ctx -> TAppExp (e1 ctx, ty ctx) }
  | e=aExpr { e }

aExpr :
    i=INTV { fun ctx -> IConst i }
  | TRUE { fun ctx -> BConst true }
  | FALSE { fun ctx -> BConst false }
  | id=LCID { fun ctx -> Var (name2index ctx id) }
  | LPAREN e=expr RPAREN { e }
  | LPAREN e=expr COLON src=ty DARROW tgt=ty RPAREN
      { fun ctx -> CastExp (e ctx, src ctx , tgt ctx) }

ifExpr :
    IF e1=expr THEN e2=expr ELSE e3=expr
      { fun ctx -> IfExp (e1 ctx, e2 ctx, e3 ctx) }

letExpr :
    LET id=LCID plist=letParamList COLON ty=ty EQ e=expr IN body=expr { fun ctx ->
      let (t, ty) = plist ctx e ty in
      AppExp(FunExp(id, ty, body ((id,VDecl ty)::ctx)), t)
    }

funExpr :
    FUN plist=funParamList RARROW body=expr { fun ctx -> plist ctx body }

/*
LetRecExpr :
    LET REC LCID EQ FUN LCID RARROW Expr IN Expr { LetRecExp ($3, $6, $8, $10) }
*/

funParamList :
    LPAREN id=LCID COLON ty=ty RPAREN { fun ctx t -> let ty = ty ctx in FunExp(id, ty, t ((id,VDecl ty)::ctx)) }
  | id=STVarID { fun ctx t -> TSFunExp(id, t ((id,STVar)::ctx)) }
  | id=GTVarID { fun ctx t -> TGFunExp(id, t ((id,GTVar)::ctx)) }
  | LPAREN id=LCID COLON ty=ty RPAREN rest=funParamList { fun ctx t -> let ty = ty ctx in FunExp(id, ty, rest ((id,VDecl ty)::ctx) t) }
  | id=STVarID rest=funParamList { fun ctx t -> TSFunExp(id, rest ((id,STVar)::ctx) t) }
  | id=GTVarID rest=funParamList { fun ctx t -> TGFunExp(id, rest ((id,GTVar)::ctx) t) }

letParamList :
    /* empty */ { fun ctx t ty -> (t ctx, ty ctx) }
  | LPAREN id=LCID COLON ty=ty RPAREN rest=letParamList { fun ctx t ty ->
       let ty' = ty ctx in
       let (t', ty'') = rest ((id, VDecl ty')::ctx) t ty in
       FunExp(id, ty', t'), Arr(ty', typeShift (-1) 0 ty'')
    }
  | id=STVarID rest=letParamList { fun ctx t ty ->
       let (t', ty') = rest ((id,STVar)::ctx) t ty in
       TSFunExp(id, t'), Forall(id, ty')
    }
  | id=GTVarID rest=letParamList { fun ctx t ty ->
       let (t', ty') = rest ((id,GTVar)::ctx) t ty in
       TGFunExp(id, t'), Forall(id, ty')
    }

ty :
  | ty1=aType RARROW ty2=ty { fun ctx -> Arr(ty1 ctx, ty2 ctx) }
  | ALL id=neTVSeq DOT ty=ty { fun ctx -> id ctx ty }
  | ty=aType { ty }

neTVSeq : /* nonempty type var sequence */
    STVarID { fun ctx ty -> Forall($1, ty (($1,STVar)::ctx)) }
  | GTVarID { fun ctx ty -> Forall($1, ty (($1,GTVar)::ctx)) }
  | STVarID neTVSeq { fun ctx ty -> Forall($1, $2 (($1,STVar)::ctx) ty) }
  | GTVarID neTVSeq { fun ctx ty -> Forall($1, $2 (($1,GTVar)::ctx) ty) }

aType :
    INT { fun ctx -> Int }
  | BOOL { fun ctx -> Bool }
  | AST { fun ctx -> Dyn }
  | id=GTVarID { fun ctx -> TyVar (name2index ctx id) }
  | id=STVarID { fun ctx -> TyVar (name2index ctx id) }
  | LPAREN ty=ty RPAREN { ty }
