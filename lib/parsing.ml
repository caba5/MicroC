(* https://www.baturin.org/blog/declarative-parse-error-reporting-with-menhir/ *)
open Lexing
module I = Parser.MenhirInterpreter

exception Syntax_error of Location.lexeme_pos * string

let get_parse_error env =
  match I.stack env with
  | (lazy MenhirLib.General.Nil) -> "Invalid syntax"
  | (lazy (MenhirLib.General.Cons (I.Element (state, _, _, _), _))) -> (
      try
        let err = Parser_messages.message (I.number state) in
        err ^ "\t(Error in state: " ^ string_of_int (I.number state) ^ ")"
      with Not_found -> "invalid syntax")

let rec parse_b lexbuf (checkpoint : Ast.program I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let token = Scanner.next_token lexbuf in
      let startp = lexbuf.lex_start_p and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse_b lexbuf checkpoint
  | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse_b lexbuf checkpoint
  | I.HandlingError _env ->
      let pos = Location.to_lexeme_position lexbuf in
      let err = get_parse_error _env in
      raise (Syntax_error (pos, err))
  | I.Accepted v -> v
  | I.Rejected ->
      (* Should not get here *)
      raise
        (Syntax_error
           ( Location.dummy_lexeme_pos,
             "invalid syntax (parser rejected the input)" ))

let parse file lexbuf =
  Lexing.set_filename lexbuf file;
  parse_b lexbuf (Parser.Incremental.program lexbuf.lex_curr_p)
