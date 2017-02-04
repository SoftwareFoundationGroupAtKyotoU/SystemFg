open Eval
       
let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  let newenv =
    try
      let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
      let (id, v, newenv) = eval_decl env decl in
      print_string (id ^ " = ");
      pp_val v;
      print_newline();
      newenv
    with Parsing.Parse_error -> print_string "Parse error\n"; env
       | Failure s -> print_string s; print_newline(); env
       | _ -> env
  in 
  read_eval_print newenv
                    
let initial_env = Empty

let _ = read_eval_print initial_env
