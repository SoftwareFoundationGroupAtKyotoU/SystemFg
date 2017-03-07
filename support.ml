open Format

module Error =
struct

  open Lexing

  type 'a with_pos = {p:position; v:'a}

  type range = {frm:position; to_:position}

  type 'a with_ran = {r:range; v:'a}

  let dummy_range = {frm=dummy_pos; to_=dummy_pos}

  let join_range r1 r2 = {frm=r1.frm; to_=r2.to_}

  let print_pos ppf pos =
    (if pos.pos_fname = "" then
       fprintf ppf "line %d, character %d"
     else
       fprintf ppf "File \"%s\", line %d, character %d" pos.pos_fname)
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)

  let string_of_pos pos =
    (if pos.pos_fname = "" then
       Printf.sprintf "line %d, character %d"
     else
       Printf.sprintf "File \"%s\", line %d, character %d" pos.pos_fname)
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)

  let print_ran ppf {frm=pos1; to_=pos2} =
    (if pos1.pos_fname = "" then
	fprintf ppf "line %d, character %d -- line %d, character %d"
      else
	fprintf ppf "File \"%s\", line %d, character %d -- line %d, character %d" pos1.pos_fname)
      pos1.pos_lnum
      (pos1.pos_cnum - pos1.pos_bol)
      pos2.pos_lnum
      (pos2.pos_cnum - pos2.pos_bol)

  let err ppf s =
    fprintf ppf "\n%s\n" s;
    exit 0

  let errAt ppf pos s =
    fprintf ppf "\n%a" print_pos pos;
    err ppf s

  let errBtw ppf ran s =
    fprintf ppf "\n%a" print_ran ran;
    err ppf s

  let warning ppf s =
    fprintf ppf "\n%s\n@?" s

  let warningAt ppf pos s =
    fprintf ppf "\n%a%a" print_pos pos warning s

  let warningBtw ppf ran s =
    fprintf ppf "\n%a%a" print_ran ran warning s

  let err s = err std_formatter s
  let errAt pos s = errAt std_formatter pos s
  let errBtw ran s = errBtw std_formatter ran s

  let warning s = warning std_formatter s
  let warningAt pos s = warningAt std_formatter pos s
  let warningBtw ran s = warningBtw std_formatter ran s
end
