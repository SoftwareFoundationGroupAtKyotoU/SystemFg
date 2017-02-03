{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("fun", Parser.FUN);
  ("if", Parser.IF);
  ("in", Parser.IN);
  ("let", Parser.LET);
  ("rec", Parser.REC);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  ("Int", Parser.INT);
  ("Bool", Parser.BOOL);
  ("All", Parser.ALL);
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| "[" { Parser.LBRACKET }
| "]" { Parser.RBRACKET }
| ";;" { Parser.SEMISEMI }
| "->" { Parser.RARROW }
| "=>" { Parser.DARROW }
| ":" { Parser.COLON }
| "." { Parser.DOT }
| "+" { Parser.PLUS }
| "*" { Parser.AST }
| "<" { Parser.LT }
| "=" { Parser.EQ }

| ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.LCID id
     }
| ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.STVarID id
     }
| '\'' ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
          Parser.GTVarID id
     }
| eof { exit 0 }


