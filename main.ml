open Eval
       
let rec read_eval_print lexeme env tyenv =
  print_string "# ";
  flush stdout;
  let newenv, newtyenv =
    try
      let decl = Parser.toplevel Lexer.main lexeme in
      let (id, v, newenv, newtyenv) = eval_decl env tyenv (decl tyenv) in
      print_string (id ^ " = ");
      pp_val v;
      print_newline();
      newenv, newtyenv
    with Parsing.Parse_error -> print_string "Parse error\n"; env, tyenv
       | Failure s -> print_string s; print_newline(); env, tyenv
       | _ -> env, tyenv
  in 
  read_eval_print lexeme newenv newtyenv
                    
let initial_env = Empty
let initial_tenv = []

let () = read_eval_print (Lexing.from_channel stdin) initial_env initial_tenv

