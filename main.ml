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
      let decl, ty = Typing.FG.translateDecl tyenv (decl tyenv) in
      assert(ty = Typing.FC.typingDecl tyenv decl);
      let (id, v, newenv, newtyenv) = Eval.eval_decl env tyenv decl in
      pr std_formatter "%s : %a = %a\n" id (Pp.print_type tyenv) ty Pp.print_val v;
      newenv, newtyenv
    with
      Gtfparser.Error ->
       pr std_formatter "Parse error\n";
       Lexing.flush_input lexeme;
       env, tyenv
    | Syntax.UnboundVar (p, s) -> warningAt p s; env, tyenv
    | ImplBug (p, s) ->
       pr std_formatter "\n%a\nImplementation bug (%s)" print_pos p s;
       exit 0
    | ImplBugV (p, s, v) ->
       pr std_formatter "\n%a\nImplementation bug (%s): %a" print_pos p s Pp.print_val v;
       exit 0
    | ImplBugRanV (r, s, v) ->
       pr std_formatter "\n%a\nImplementation bug (%s): %a" print_ran r s Pp.print_val v;
       exit 0
    | Typing.TypeError (p, s, tyenv, ty) ->
       pr std_formatter ("\n%a\n" ^^ s) print_pos p (Pp.print_type tyenv) ty;
       env, tyenv
    | Typing.TypeError2 (p, s, tyenv, ty1, ty2) ->
       pr std_formatter ("\n%a\n" ^^ s) print_pos p (Pp.print_type tyenv) ty1 (Pp.print_type tyenv) ty2;
       env, tyenv
    | Blame (r, plr, v, s) ->
       (match plr with
          Pos -> pr std_formatter "\n%a\nBlame on the expression side: %a => %s\n" print_ran r Pp.print_val v s
        | Neg -> pr std_formatter "\n%a\nBlame on the environment side: %a => %s\n" print_ran r Pp.print_val v s);
       env, tyenv
    | Failure s ->
       warning (Format.sprintf "Uncaught failure %s\n" s); env, tyenv
  in
  read_eval_print lexeme newenv newtyenv

let () = read_eval_print (Lexing.from_channel stdin) Eval.initial_env Eval.initial_tenv

