type lexeme_pos = {
  file : string; (* Reference to the module *)
  line : int;
  start_column : int;
  end_column : int;
}
[@@deriving show, ord, eq]

type code_pos = {
  file : string; (* Reference to the module *)
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
}
[@@deriving show, ord, eq]

let to_lexeme_position lexbuf =
  let startp = Lexing.lexeme_start_p lexbuf in
  let endp = Lexing.lexeme_end_p lexbuf in
  let line = startp.Lexing.pos_lnum in
  let file = startp.Lexing.pos_fname in
  let start_column = startp.Lexing.pos_cnum - startp.Lexing.pos_bol + 1 in
  let end_column = endp.Lexing.pos_cnum - endp.Lexing.pos_bol in
  { file; line; start_column; end_column }

let to_code_position (start_position, end_position) =
  let file = start_position.Lexing.pos_fname in
  let start_line = start_position.Lexing.pos_lnum in
  let end_line = end_position.Lexing.pos_lnum in
  let start_column =
    start_position.Lexing.pos_cnum - start_position.Lexing.pos_bol + 1
  in
  let end_column = end_position.Lexing.pos_cnum - end_position.Lexing.pos_bol in
  { file; start_line; end_line; start_column; end_column }

let dummy_code_pos =
  { file = ""; start_line = 0; start_column = 0; end_line = 0; end_column = 0 }

let dummy_lexeme_pos = { file = ""; line = 0; start_column = 0; end_column = 0 }
