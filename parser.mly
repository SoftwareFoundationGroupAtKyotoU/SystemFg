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
    Expr SEMISEMI { fun ctx -> Prog ($1 ctx) }
  | LET LCID LetParamList COLON Type EQ Expr SEMISEMI { fun ctx ->
      let t, ty = $3 ctx $7 $5 in
      Decl ($2, ty, t) }
/*
  | LET REC ID EQ FUN ID RARROW Expr SEMISEMI { RecDecl ($3, $6, $8) }
*/

Expr :
    IfExpr { $1 }
  | FunExpr { $1 }
  | LetExpr { $1 }
/*  | LetRecExpr { $1 } */
  | LTExpr { $1 }

LTExpr : 
    PExpr LT PExpr { fun ctx -> BinOp (Lt, $1 ctx, $3 ctx) }
  | PExpr { $1 }

PExpr :
    PExpr PLUS MExpr { fun ctx -> BinOp (Plus, $1 ctx, $3 ctx) }
  | MExpr { $1 }

MExpr : 
    MExpr AST AppExpr { fun ctx -> BinOp (Mult, $1 ctx, $3 ctx) }
  | AppExpr { $1 }

AppExpr :
    AppExpr AExpr { fun ctx -> AppExp ($1 ctx, $2 ctx) }
  | AppExpr LBRACKET Type RBRACKET { fun ctx -> TAppExp ($1 ctx, $3 ctx) }
  | AExpr { $1 }

AExpr :
    INTV { fun ctx -> IConst $1 }
  | TRUE { fun ctx -> BConst true }
  | FALSE { fun ctx -> BConst false }
  | LCID { fun ctx -> Var (name2index ctx $1) }
  | LPAREN Expr RPAREN { $2 }
  | LPAREN Expr COLON Type DARROW Type RPAREN { fun ctx -> CastExp ($2 ctx, $4 ctx , $6 ctx) }

IfExpr :
    IF Expr THEN Expr ELSE Expr { fun ctx -> IfExp ($2 ctx, $4 ctx, $6 ctx) }

LetExpr :
    LET LCID LetParamList COLON Type EQ Expr IN Expr { fun ctx ->
      let (t, ty) = $3 ctx $7 $5 in
      AppExp(FunExp($2, ty, $9 (($2,VDecl ty)::ctx)), t)
    }

FunExpr :
    FUN FunParamList RARROW Expr { fun ctx -> $2 ctx $4 }

/*
LetRecExpr :
    LET REC LCID EQ FUN LCID RARROW Expr IN Expr { LetRecExp ($3, $6, $8, $10) }
*/

FunParamList :
    LPAREN LCID COLON Type RPAREN { fun ctx t -> let ty = $4 ctx in FunExp($2, ty, t (($2,VDecl ty)::ctx)) }
  | STVarID { fun ctx t -> TSFunExp($1, t (($1,STVar)::ctx)) }
  | GTVarID { fun ctx t -> TGFunExp($1, t (($1,GTVar)::ctx)) }
  | LPAREN LCID COLON Type RPAREN FunParamList { fun ctx t -> let ty = $4 ctx in FunExp($2, ty, $6 (($2,VDecl ty)::ctx) t) }
  | STVarID FunParamList { fun ctx t -> TSFunExp($1, $2 (($1,STVar)::ctx) t) }
  | GTVarID FunParamList { fun ctx t -> TGFunExp($1, $2 (($1,GTVar)::ctx) t) }

LetParamList :
    /* empty */ { fun ctx t ty -> (t ctx, ty ctx) }
  | LPAREN LCID COLON Type RPAREN LetParamList { fun ctx t ty ->
       let ty' = $4 ctx in
       let (t', ty'') = $6 (($2,VDecl ty')::ctx) t ty in
       FunExp($2, ty', t'), Arr(ty', typeShift (-1) 0 ty'')
    }
  | STVarID LetParamList { fun ctx t ty ->
       let (t', ty') = $2 (($1,STVar)::ctx) t ty in
       TSFunExp($1, t'), Forall($1, ty')
    }
  | GTVarID LetParamList { fun ctx t ty ->
       let (t', ty') = $2 (($1,GTVar)::ctx) t ty in
       TGFunExp($1, t'), Forall($1, ty')
    }

Type :
  | AType RARROW Type { fun ctx -> Arr($1 ctx, $3 ctx) }
  | ALL NETVSeq DOT Type { fun ctx -> $2 ctx $4 }
  | AType { $1 }

NETVSeq : /* nonempty type var sequence */
    STVarID { fun ctx ty -> Forall($1, ty (($1,STVar)::ctx)) }
  | GTVarID { fun ctx ty -> Forall($1, ty (($1,GTVar)::ctx)) }
  | STVarID NETVSeq { fun ctx ty -> Forall($1, $2 (($1,STVar)::ctx) ty) }
  | GTVarID NETVSeq { fun ctx ty -> Forall($1, $2 (($1,GTVar)::ctx) ty) }

AType :
    INT { fun ctx -> Int }
  | BOOL { fun ctx -> Bool }
  | AST { fun ctx -> Dyn }
  | GTVarID { fun ctx -> TyVar (name2index ctx $1) }
  | STVarID { fun ctx -> TyVar (name2index ctx $1) }
  | LPAREN Type RPAREN { $2 }