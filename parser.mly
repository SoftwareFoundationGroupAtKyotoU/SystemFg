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
  | LET LCID COLON Type EQ Expr SEMISEMI { Decl ($2, $4, $6) }
/*
  | LET REC ID EQ FUN ID RARROW Expr SEMISEMI { RecDecl ($3, $6, $8) }
*/

Expr :
    IfExpr { $1 }
  | FunExpr { $1 }
  | STAbsExpr { $1 }
  | GTAbsExpr { $1 }
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
    LET LCID COLON Type EQ Expr IN Expr { AppExp(FunExp($2, $4, $8), $6) }

FunExpr :
    FUN LPAREN LCID COLON Type RPAREN RARROW Expr { FunExp ($3, $5, $8) }

STAbsExpr :
    FUN STVarID RARROW Expr { TSFunExp ($2, $4) }
    
GTAbsExpr :
    FUN GTVarID RARROW Expr { TGFunExp ($2, $4) }

/*
LetRecExpr :
    LET REC LCID EQ FUN LCID RARROW Expr IN Expr { LetRecExp ($3, $6, $8, $10) }
*/


Type :
  | AType RARROW Type { Arr($1, $3) }
  | ALL GTVarID DOT Type { Forall($2, $4) }
  | AType { $1 }

AType :
    INT { Int }
  | BOOL { Bool }
  | AST { Dyn }
  | GTVarID { TyVar $1 }
  | STVarID { TyVar $1 }
  | LPAREN Type RPAREN { $2 }