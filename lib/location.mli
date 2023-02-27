type lexeme_pos = {
  file : string;
  line : int;
  start_column : int;
  end_column : int;
}
[@@deriving show, ord, eq]

type code_pos = {
  file : string;
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
}
[@@deriving show, ord, eq]

val to_lexeme_position : Lexing.lexbuf -> lexeme_pos
val to_code_position : Lexing.position * Lexing.position -> code_pos
val dummy_code_pos : code_pos
val dummy_lexeme_pos : lexeme_pos
