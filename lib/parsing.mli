exception Syntax_error of Location.lexeme_pos * string

val parse : string -> Lexing.lexbuf -> Ast.program
