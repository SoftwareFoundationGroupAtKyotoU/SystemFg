%{
open Syntax
%}

%token LPAREN RPAREN LBRACKET RBRACKET SEMISEMI RARROW DARROW COLON DOT 
%token PLUS AST LT EQ
%token IF THEN ELSE TRUE FALSE LET IN FUN REC INT BOOL ALL

%token <int> INTV
%token <Syntax.id> LCID
%token <Syntax.id> STVarID
%token <Syntax.id> GTVarID

%start toplevel
%type <Syntax.tyenv -> Syntax.program> toplevel
%%

toplevel :
    expr SEMISEMI { fun ctx -> Prog ($1 ctx) }
  | LET LCID letParamList COLON ty EQ expr SEMISEMI { fun ctx ->
      let t, ty = $3 ctx $7 $5 in
      Decl ($2, ty, t) }
/*
  | LET REC ID EQ FUN ID RARROW Expr SEMISEMI { RecDecl ($3, $6, $8) }
*/

expr :
    ifExpr { $1 }
  | funExpr { $1 }
  | letExpr { $1 }
/*  | letRecExpr { $1 } */
  | lTExpr { $1 }

lTExpr : 
    pExpr LT pExpr { fun ctx -> BinOp (Lt, $1 ctx, $3 ctx) }
  | pExpr { $1 }

pExpr :
    pExpr PLUS mExpr { fun ctx -> BinOp (Plus, $1 ctx, $3 ctx) }
  | mExpr { $1 }

mExpr : 
    mExpr AST appExpr { fun ctx -> BinOp (Mult, $1 ctx, $3 ctx) }
  | appExpr { $1 }

appExpr :
    appExpr aExpr { fun ctx -> AppExp ($1 ctx, $2 ctx) }
  | appExpr LBRACKET ty RBRACKET { fun ctx -> TAppExp ($1 ctx, $3 ctx) }
  | aExpr { $1 }

aExpr :
    INTV { fun ctx -> IConst $1 }
  | TRUE { fun ctx -> BConst true }
  | FALSE { fun ctx -> BConst false }
  | LCID { fun ctx -> Var (name2index ctx $1) }
  | LPAREN expr RPAREN { $2 }
  | LPAREN expr COLON ty DARROW ty RPAREN { fun ctx -> CastExp ($2 ctx, $4 ctx , $6 ctx) }

ifExpr :
    IF expr THEN expr ELSE expr { fun ctx -> IfExp ($2 ctx, $4 ctx, $6 ctx) }

letExpr :
    LET LCID letParamList COLON ty EQ expr IN expr { fun ctx ->
      let (t, ty) = $3 ctx $7 $5 in
      AppExp(FunExp($2, ty, $9 (($2,VDecl ty)::ctx)), t)
    }

funExpr :
    FUN funParamList RARROW expr { fun ctx -> $2 ctx $4 }

/*
LetRecExpr :
    LET REC LCID EQ FUN LCID RARROW Expr IN Expr { LetRecExp ($3, $6, $8, $10) }
*/

funParamList :
    LPAREN LCID COLON ty RPAREN { fun ctx t -> let ty = $4 ctx in FunExp($2, ty, t (($2,VDecl ty)::ctx)) }
  | STVarID { fun ctx t -> TSFunExp($1, t (($1,STVar)::ctx)) }
  | GTVarID { fun ctx t -> TGFunExp($1, t (($1,GTVar)::ctx)) }
  | LPAREN LCID COLON ty RPAREN funParamList { fun ctx t -> let ty = $4 ctx in FunExp($2, ty, $6 (($2,VDecl ty)::ctx) t) }
  | STVarID funParamList { fun ctx t -> TSFunExp($1, $2 (($1,STVar)::ctx) t) }
  | GTVarID funParamList { fun ctx t -> TGFunExp($1, $2 (($1,GTVar)::ctx) t) }

letParamList :
    /* empty */ { fun ctx t ty -> (t ctx, ty ctx) }
  | LPAREN LCID COLON ty RPAREN letParamList { fun ctx t ty ->
       let ty' = $4 ctx in
       let (t', ty'') = $6 (($2,VDecl ty')::ctx) t ty in
       FunExp($2, ty', t'), Arr(ty', typeShift (-1) 0 ty'')
    }
  | STVarID letParamList { fun ctx t ty ->
       let (t', ty') = $2 (($1,STVar)::ctx) t ty in
       TSFunExp($1, t'), Forall($1, ty')
    }
  | GTVarID letParamList { fun ctx t ty ->
       let (t', ty') = $2 (($1,GTVar)::ctx) t ty in
       TGFunExp($1, t'), Forall($1, ty')
    }

ty :
  | aType RARROW ty { fun ctx -> Arr($1 ctx, $3 ctx) }
  | ALL neTVSeq DOT ty { fun ctx -> $2 ctx $4 }
  | aType { $1 }

neTVSeq : /* nonempty type var sequence */
    STVarID { fun ctx ty -> Forall($1, ty (($1,STVar)::ctx)) }
  | GTVarID { fun ctx ty -> Forall($1, ty (($1,GTVar)::ctx)) }
  | STVarID neTVSeq { fun ctx ty -> Forall($1, $2 (($1,STVar)::ctx) ty) }
  | GTVarID neTVSeq { fun ctx ty -> Forall($1, $2 (($1,GTVar)::ctx) ty) }

aType :
    INT { fun ctx -> Int }
  | BOOL { fun ctx -> Bool }
  | AST { fun ctx -> Dyn }
  | GTVarID { fun ctx -> TyVar (name2index ctx $1) }
  | STVarID { fun ctx -> TyVar (name2index ctx $1) }
  | LPAREN ty RPAREN { $2 }