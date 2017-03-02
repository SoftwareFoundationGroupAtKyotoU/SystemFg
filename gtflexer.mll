{
let reservedWords = [
  (* Keywords *)
  ("else", Gtfparser.ELSE);
  ("false", Gtfparser.FALSE);
  ("fun", Gtfparser.FUN);
  ("if", Gtfparser.IF);
  ("in", Gtfparser.IN);
  ("let", Gtfparser.LET);
  ("rec", Gtfparser.REC);
  ("then", Gtfparser.THEN);
  ("true", Gtfparser.TRUE);
  ("Int", Gtfparser.INT);
  ("Bool", Gtfparser.BOOL);
  ("All", Gtfparser.ALL);
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Gtfparser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Gtfparser.LPAREN }
| ")" { Gtfparser.RPAREN }
| "[" { Gtfparser.LBRACKET }
| "]" { Gtfparser.RBRACKET }
| ";;" { Gtfparser.SEMISEMI }
| "->" { Gtfparser.RARROW }
| "=>" { Gtfparser.DARROW }
| ":" { Gtfparser.COLON }
| "." { Gtfparser.DOT }
| "+" { Gtfparser.PLUS }
| "*" { Gtfparser.AST }
| "<" { Gtfparser.LT }
| "=" { Gtfparser.EQ }

| ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Gtfparser.LCID id
     }
| ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Gtfparser.UCID id
     }
| '\'' ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
          Gtfparser.PRIMEUCID id
     }
| eof { exit 0 }


