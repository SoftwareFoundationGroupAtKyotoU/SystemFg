open Support.Error
open Format
open Eval
open Typing
open Pp

let pr = fprintf

let rec read_eval_print lexeme env tyenv =
  pr std_formatter "# @?";  (* @? to flush the output buffer *)
  flush stdout;
  let newenv, newtyenv =
    try
      let decl = Gtfparser.toplevel Gtflexer.main lexeme in
      let decl, tye = Typing.FG.translateDecl tyenv (decl tyenv) in
      let ty = Typing.FC.typingDecl tyenv decl in
      (* tye is inferred by translation;
         ty is inferred by typechecking;
         they should agree *)
      (* pr std_formatter "DEBUG: %a vs %a\n" (print_type tyenv) ty (print_type tyenv) tye; *)
      assert(ty = tye);
      pr std_formatter "\027[32mDEBUG: %a\027[0m\n" FC.print_rawdecl decl;
      let (id, v, newenv, newtyenv) = match decl with
          Syntax.FC.Prog e ->
            let v = eval e env in
            ("-", v, env, tyenv)
        | Syntax.FC.Decl(id, e) ->
           let v = eval e env in
           (id, v, VB(v, env), (id, Syntax.VDecl ty)::tyenv) in
      pr std_formatter "%s : %a = %a\n" id (Pp.print_type tyenv) ty Pp.print_val v;
      newenv, newtyenv
    with
      (* Soft errors *)
      Gtfparser.Error ->
       pr std_formatter "Parse error\n";
       Lexing.flush_input lexeme;
       env, tyenv
    | Syntax.UnboundVar (p, s) -> warningAt p s; env, tyenv
    | Typing.TypeError (p, s, tyenv_error, ty) ->
       pr std_formatter ("\n%a\n" ^^ s) print_pos p (Pp.print_type tyenv_error) ty;
       pr std_formatter "\n";
       env, tyenv
    | Typing.TypeError2 (p, s, tyenv_error, ty1, ty2) ->
       pr std_formatter ("\n%a\n" ^^ s) print_pos p (Pp.print_type tyenv_error) ty1 (Pp.print_type tyenv_error) ty2;
       pr std_formatter "\n";
       env, tyenv
    | Blame (r, plr, (Tagged(_,_,rv) as v), s) ->
       (match plr with
          Pos -> pr std_formatter "\n%a\nBlame on the expression side: %a => %s\nValue source: %a\n"
                    print_ran r Pp.print_val v s print_ran rv 
        | Neg -> pr std_formatter "\n%a\nBlame on the environment side: %a => %s\nValue source: %a\n"
                    print_ran r Pp.print_val v s print_ran rv);
       env, tyenv
    (* Fatal errors *)
    | ImplBug (p, s) ->
       pr std_formatter "\n%a\nImplementation bug (%s)\n" print_pos p s;
       exit 0
    | ImplBugV (p, s, v) ->
       pr std_formatter "\n%a\nImplementation bug (%s): %a\n" print_pos p s Pp.print_val v;
       exit 0
    | ImplBugRanV (r, s, v) ->
       pr std_formatter "\n%a\nImplementation bug (%s): %a\n" print_ran r s Pp.print_val v;
       exit 0
  in
  read_eval_print lexeme newenv newtyenv

let () = read_eval_print (Lexing.from_channel stdin) Eval.initial_env Eval.initial_tenv

