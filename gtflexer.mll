{
open Lexing
open Support.Error

let reservedWords = [
  (* Keywords *)
  ("else", fun r-> Gtfparser.ELSE r);
  ("false", fun r-> Gtfparser.FALSE r);
  ("fun", fun r-> Gtfparser.FUN r);
  ("if", fun r-> Gtfparser.IF r);
  ("in", fun r-> Gtfparser.IN r);
  ("let", fun r-> Gtfparser.LET r);
  ("rec", fun r-> Gtfparser.REC r);
  ("then", fun r-> Gtfparser.THEN r);
  ("true", fun r-> Gtfparser.TRUE r);
  ("match", fun r -> Gtfparser.MATCH r);
  ("with", fun r -> Gtfparser.WITH r);
  (* deprecated *)
  ("Int", fun r-> Gtfparser.INT r);
  ("Bool", fun r-> Gtfparser.BOOL r);
  (* end deprecated *)
  ("All", fun r-> Gtfparser.ALL r);
  ("int", fun r-> Gtfparser.INT r);
  ("bool", fun r-> Gtfparser.BOOL r);
  ("list", fun r-> Gtfparser.LIST r);
]

let range lexbuf =
  {frm=Lexing.lexeme_start_p lexbuf; to_=Lexing.lexeme_end_p lexbuf}
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\t']+     { main lexbuf }
  (* ignore spacing and newline characters *)
| [' ' '\t']* '\n'    { new_line lexbuf; main lexbuf }
  (* ignore // and the following characters until the end of the line *)
| "//" [^ '\n']* '\n' { new_line lexbuf; main lexbuf } 

| "-"? ['0'-'9']+
    { Gtfparser.INTV {r=range lexbuf; v=int_of_string (Lexing.lexeme lexbuf)} }

| "(" { Gtfparser.LPAREN (range lexbuf)}
| ")" { Gtfparser.RPAREN (range lexbuf)}
| "[" { Gtfparser.LBRACKET (range lexbuf)}
| "]" { Gtfparser.RBRACKET (range lexbuf)}
| ";;" { Gtfparser.SEMISEMI (range lexbuf)}
| "::" { Gtfparser.COLCOL (range lexbuf)}
| "->" { Gtfparser.RARROW (range lexbuf)}
| "=>" { Gtfparser.DARROW (range lexbuf)}
| ":" { Gtfparser.COLON (range lexbuf)}
| "." { Gtfparser.DOT (range lexbuf)}
| "+" { Gtfparser.PLUS (range lexbuf)}
| "*" { Gtfparser.AST (range lexbuf)}
| "<" { Gtfparser.LT (range lexbuf)}
| "=" { Gtfparser.EQ (range lexbuf)}
| "@" { Gtfparser.AT (range lexbuf)}
| ";" { Gtfparser.SEMI (range lexbuf)}
| "|" { Gtfparser.BAR (range lexbuf)}

| ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        (List.assoc id reservedWords) (range lexbuf)
      with
      _ -> Gtfparser.LCID {v=id; r=range lexbuf}
     }
| ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        (List.assoc id reservedWords) (range lexbuf)
      with
      _ -> Gtfparser.UCID {v=id; r=range lexbuf}
     }
| '\'' ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
          Gtfparser.PRIMEUCID {v=id; r=range lexbuf}
     }
| eof { exit 0 }


