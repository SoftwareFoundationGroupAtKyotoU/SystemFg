open Printf

module Error =
struct

  open Lexing

  type 'a with_pos = {p:position; v:'a}

  type range = {frm:position; to_:position}

  let join_range r1 r2 = {frm=r1.frm; to_=r2.to_}

  let print_pos pos =
    (if pos.pos_fname = "" then
      eprintf "line %d, character %d"
     else
       eprintf "File \"%s\", line %d, character %d" pos.pos_fname)
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)

  let string_of_pos pos =
    (if pos.pos_fname = "" then
       Printf.sprintf "line %d, character %d"
     else
       Printf.sprintf "File \"%s\", line %d, character %d" pos.pos_fname)
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)

  let print_2pos pos1 pos2 =
    (if pos1.pos_fname = "" then
	eprintf "line %d, character %d -- line %d, character %d"
      else
	eprintf "File \"%s\", line %d, character %d -- line %d, character %d" pos1.pos_fname)
      pos1.pos_lnum
      (pos1.pos_cnum - pos1.pos_bol)
      pos2.pos_lnum
      (pos2.pos_cnum - pos2.pos_bol)

  let err s =
    eprintf "\n%s\n" s;
    failwith ""

  let errAt pos s =
    eprintf "\n";
    print_pos pos;
    err s

  let errBtw pos1 pos2 s =
    eprintf "\n";
    print_2pos pos1 pos2;
    err s

  let warning s =
    eprintf "\n%s\n" s;
    flush stderr

  let warningAt pos s =
    eprintf "\n";
    print_pos pos;
    warning s

  let warningBtw pos1 pos2 s =
    eprintf "\n";
    print_2pos pos1 pos2;
    warning s
end
