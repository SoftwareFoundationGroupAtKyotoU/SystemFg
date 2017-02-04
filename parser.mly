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
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Prog $1 }
  | LET LCID LetParamList COLON Type EQ Expr SEMISEMI {
      let t, ty = $3 $7 $5 in
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
    PExpr LT PExpr { BinOp (Lt, $1, $3) }
  | PExpr { $1 }

PExpr :
    PExpr PLUS MExpr { BinOp (Plus, $1, $3) }
  | MExpr { $1 }

MExpr : 
    MExpr AST AppExpr { BinOp (Mult, $1, $3) }
  | AppExpr { $1 }

AppExpr :
    AppExpr AExpr { AppExp ($1, $2) }
  | AppExpr LBRACKET Type RBRACKET { TAppExp ($1, $3) }
  | AExpr { $1 }

AExpr :
    INTV { IConst $1 }
  | TRUE { BConst true }
  | FALSE { BConst false }
  | LCID { Var $1 }
  | LPAREN Expr RPAREN { $2 }
  | LPAREN Expr COLON Type DARROW Type RPAREN { CastExp ($2, $4, $6) }

IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }

LetExpr :
    LET LCID LetParamList COLON Type EQ Expr IN Expr {
      let (t, ty) = $3 $7 $5 in
      AppExp(FunExp($2, ty, $9), t)
    }

FunExpr :
    FUN FunParamList RARROW Expr { $2 $4 }

/*
LetRecExpr :
    LET REC LCID EQ FUN LCID RARROW Expr IN Expr { LetRecExp ($3, $6, $8, $10) }
*/

FunParamList :
    LPAREN LCID COLON Type RPAREN { fun t -> FunExp($2, $4, t) }
  | STVarID { fun t -> TSFunExp($1, t) }
  | GTVarID { fun t -> TGFunExp($1, t) }
  | LPAREN LCID COLON Type RPAREN FunParamList { fun t -> FunExp($2, $4, $6 t) }
  | STVarID FunParamList { fun t -> TSFunExp($1, $2 t) }
  | GTVarID FunParamList { fun t -> TGFunExp($1, $2 t) }

LetParamList :
    /* empty */ { fun t ty -> (t, ty) }
  | LPAREN LCID COLON Type RPAREN LetParamList { fun t ty ->
       let (t', ty') = $6 t ty in
       FunExp($2, $4, t'), ty'
    }
  | STVarID LetParamList { fun t ty ->
       let (t', ty') = $2 t ty in
       TSFunExp($1, t'), ty'
    }
  | GTVarID LetParamList { fun t ty ->
       let (t', ty') = $2 t ty in
       TGFunExp($1, t'), ty'
    }

Type :
  | AType RARROW Type { Arr($1, $3) }
  | ALL NETVSeq DOT Type { $2 $4 }
  | AType { $1 }

NETVSeq : /* nonempty type var sequence */
    STVarID { fun t -> Forall($1, t) }
  | GTVarID { fun t -> Forall($1, t) }
  | STVarID NETVSeq { fun t -> Forall($1, $2 t) }
  | GTVarID NETVSeq { fun t -> Forall($1, $2 t) }

AType :
    INT { Int }
  | BOOL { Bool }
  | AST { Dyn }
  | GTVarID { TyVar $1 }
  | STVarID { TyVar $1 }
  | LPAREN Type RPAREN { $2 }