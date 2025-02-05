open Format
open Lexing

let file = Sys.argv.(1)

let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  set_filename lb file ;
  try
    let prog = Kawaparser.program Kawalexer.token lb in
    close_in c;
    Typechecker.typecheck_prog prog;
    Interpreter.exec_prog prog;

    Gast.export_to_image prog "output.png";

    exit 0
  with
  | Typechecker.Error s ->
     eprintf "type error: %s@." s;
     exit 1
  | Kawalexer.Error s ->
     eprintf "lexical error: %s@." s;
     report (lexeme_start_p lb, lexeme_end_p lb);
     exit 1
  | Kawaparser.Error ->
     eprintf "syntax error@.";
     report (lexeme_start_p lb, lexeme_end_p lb);
     exit 1
  | Interpreter.Error s ->
     eprintf "interpreter error: %s@." s;
     exit 1
  | e ->
     eprintf "Anomaly: %s\n@." (Printexc.to_string e);
     exit 2
