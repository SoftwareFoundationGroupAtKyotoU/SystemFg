{
open Lexing
open Support.Error

let reservedWords = [
  (* Keywords *)
  ("else", fun p -> Gtfparser.ELSE p);
  ("false", fun p -> Gtfparser.FALSE p);
  ("fun", fun p -> Gtfparser.FUN p);
  ("if", fun p -> Gtfparser.IF p);
  ("in", fun p -> Gtfparser.IN p);
  ("let", fun p -> Gtfparser.LET p);
  ("rec", fun p -> Gtfparser.REC p);
  ("then", fun p -> Gtfparser.THEN p);
  ("true", fun p -> Gtfparser.TRUE p);
  ("Int", fun p -> Gtfparser.INT p);
  ("Bool", fun p -> Gtfparser.BOOL p);
  ("All", fun p -> Gtfparser.ALL p);
]

}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\t']+     { main lexbuf }
  (* ignore spacing and newline characters *)
| [' ' '\t']* '\n'    { print_int (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol); Printf.printf "!\n"; new_line lexbuf; main lexbuf }
  (* ignore // and the following characters until the end of the line *)
| "//" [^ '\n']* '\n' { new_line lexbuf; main lexbuf } 

| "-"? ['0'-'9']+
    { Gtfparser.INTV {p=lexeme_start_p lexbuf; v=int_of_string (Lexing.lexeme lexbuf)} }

| "(" { Gtfparser.LPAREN (lexeme_start_p lexbuf)}
| ")" { Gtfparser.RPAREN (lexeme_start_p lexbuf)}
| "[" { Gtfparser.LBRACKET (lexeme_start_p lexbuf)}
| "]" { Gtfparser.RBRACKET (lexeme_start_p lexbuf)}
| ";;" { Gtfparser.SEMISEMI (lexeme_start_p lexbuf)}
| "->" { Gtfparser.RARROW (lexeme_start_p lexbuf)}
| "=>" { Gtfparser.DARROW (lexeme_start_p lexbuf)}
| ":" { Gtfparser.COLON (lexeme_start_p lexbuf)}
| "." { Gtfparser.DOT (lexeme_start_p lexbuf)}
| "+" { Gtfparser.PLUS (lexeme_start_p lexbuf)}
| "*" { Gtfparser.AST (lexeme_start_p lexbuf)}
| "<" { Gtfparser.LT (lexeme_start_p lexbuf)}
| "=" { Gtfparser.EQ (lexeme_start_p lexbuf)}

| ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        (List.assoc id reservedWords) (lexeme_start_p lexbuf)
      with
      _ -> Gtfparser.LCID {v=id; p=lexeme_start_p lexbuf}
     }
| ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        (List.assoc id reservedWords) (lexeme_start_p lexbuf)
      with
      _ -> Gtfparser.UCID {v=id; p=lexeme_start_p lexbuf}
     }
| '\'' ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
          Gtfparser.PRIMEUCID {v=id; p=lexeme_start_p lexbuf}
     }
| eof { exit 0 }


