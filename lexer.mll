{
let reservedWords = [
  (* Keywords *)
  ("else", Parserx.ELSE);
  ("false", Parserx.FALSE);
  ("fun", Parserx.FUN);
  ("if", Parserx.IF);
  ("in", Parserx.IN);
  ("let", Parserx.LET);
  ("rec", Parserx.REC);
  ("then", Parserx.THEN);
  ("true", Parserx.TRUE);
  ("Int", Parserx.INT);
  ("Bool", Parserx.BOOL);
  ("All", Parserx.ALL);
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parserx.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parserx.LPAREN }
| ")" { Parserx.RPAREN }
| "[" { Parserx.LBRACKET }
| "]" { Parserx.RBRACKET }
| ";;" { Parserx.SEMISEMI }
| "->" { Parserx.RARROW }
| "=>" { Parserx.DARROW }
| ":" { Parserx.COLON }
| "." { Parserx.DOT }
| "+" { Parserx.PLUS }
| "*" { Parserx.AST }
| "<" { Parserx.LT }
| "=" { Parserx.EQ }

| ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parserx.LCID id
     }
| ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parserx.GTVarID id
     }
| '\'' ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
          Parserx.STVarID id
     }
| eof { exit 0 }


