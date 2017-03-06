open Eval
open Typing

let rec read_eval_print lexeme env tyenv =
  print_string "# ";
  flush stdout;
  let newenv, newtyenv =
    try
      let decl = Gtfparser.toplevel Gtflexer.main lexeme in
      let decl, ty = FG.translateDecl tyenv (decl tyenv) in
      assert(ty = FC.typingDecl tyenv decl);
      let (id, v, newenv, newtyenv) = eval_decl env tyenv decl in
      print_string (id ^ " : ");
      print_string (Syntax.string_of_ty tyenv ty);
      print_string " = ";
      pp_val v;
      print_newline();
      newenv, newtyenv
    with Gtfparser.Error ->
         print_string "Parse error\n";
         Lexing.flush_input lexeme;
         env, tyenv
       | Failure _ -> env, tyenv
  in
  read_eval_print lexeme newenv newtyenv

let () = read_eval_print (Lexing.from_channel stdin) initial_env initial_tenv

