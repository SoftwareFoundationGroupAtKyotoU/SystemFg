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

let newline lexbuf =
  let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\r']+     { main lexbuf }
  (* ignore spacing and newline characters *)
| [' ' '\009' '\012' '\r']* '\n'    { newline lexbuf; main lexbuf }
  (* ignore // and the following characters until the end of the line *)
| "//" [^ '\n']* '\n' { newline lexbuf; main lexbuf } 

| "-"? ['0'-'9']+
    { Gtfparser.INTV {p=lexbuf.lex_curr_p; v=int_of_string (Lexing.lexeme lexbuf)} }

| "(" { Gtfparser.LPAREN lexbuf.lex_curr_p}
| ")" { Gtfparser.RPAREN lexbuf.lex_curr_p}
| "[" { Gtfparser.LBRACKET lexbuf.lex_curr_p}
| "]" { Gtfparser.RBRACKET lexbuf.lex_curr_p}
| ";;" { Gtfparser.SEMISEMI lexbuf.lex_curr_p}
| "->" { Gtfparser.RARROW lexbuf.lex_curr_p}
| "=>" { Gtfparser.DARROW lexbuf.lex_curr_p}
| ":" { Gtfparser.COLON lexbuf.lex_curr_p}
| "." { Gtfparser.DOT lexbuf.lex_curr_p}
| "+" { Gtfparser.PLUS lexbuf.lex_curr_p}
| "*" { Gtfparser.AST lexbuf.lex_curr_p}
| "<" { Gtfparser.LT lexbuf.lex_curr_p}
| "=" { Gtfparser.EQ lexbuf.lex_curr_p}

| ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        (List.assoc id reservedWords) lexbuf.lex_curr_p
      with
      _ -> Gtfparser.LCID {v=id; p=lexbuf.lex_curr_p}
     }
| ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        (List.assoc id reservedWords) lexbuf.lex_curr_p
      with
      _ -> Gtfparser.UCID {v=id; p=lexbuf.lex_curr_p}
     }
| '\'' ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
          Gtfparser.PRIMEUCID {v=id; p=lexbuf.lex_curr_p}
     }
| eof { exit 0 }


