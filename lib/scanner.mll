{
    open Parser

    (* Auxiliary definitions *)
    exception Lexing_error of Location.lexeme_pos * string

    let raise_error lexbuf text =
        raise (Lexing_error ((Location.to_lexeme_position lexbuf), text))

    let create_hashtable size init =
        let tbl = Hashtbl.create size in
        List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
        tbl
    
    let keyword_table =
        create_hashtable 13 [
            ("if",      IF);
            ("return",  RETURN);
            ("else",    ELSE);
            ("for",     FOR);
            ("while",   WHILE);
			("do", 		DO);
            ("int",     INT);
            ("float",   FLOAT);
            ("char",    CHAR);
            ("void",    VOID);
            ("bool",    BOOL);
            ("NULL",    NULL);
            ("struct",  STRUCT)
        ]

    let to_special ch = match ch with
        | '\''  -> Some '\''
        | 'b'   -> Some '\b'
        | 'f'   -> Some '\x0C'  (* form feed *)
        | 't'   -> Some '\t'
        | '\\'  -> Some '\\'
        | 'r'   -> Some '\r'
        | 'n'   -> Some '\n'
        | _     -> None
}

(* Scanner specification *)

let letter = ['a'-'z' 'A'-'Z']
let number = ['0'-'9']
let hexLetter = ['A'-'F']
let exp = ['e' 'E'] ['+' '-']? number+

rule next_token = parse 
| [' ' '\t']                                       	{ next_token lexbuf }
| '\n'												{ Lexing.new_line lexbuf; next_token lexbuf }
| "true" | "false" as bool                          { VBOOL (bool_of_string bool) }
| "sizeof"                                          { SIZEOF }
| (letter | '_') (letter | '_' | number)* as id     { 
                                                        match Hashtbl.find_opt keyword_table id with
                                                        | Some keyword  -> keyword
                                                        | None          -> ID (id)
                                                    }
| number+ | "0x" (number | hexLetter)+ as intNum    { VINT (int_of_string intNum) }
| (number* '.' number+ | number+ '.' number*) exp? 
| number+ exp as floatNum                           { VFLOAT (float_of_string floatNum) }
| '\''                                              { next_char lexbuf }
| '"'                                               { next_string (Buffer.create 16) lexbuf }
| '&'                                               { AMP }
| '+'                                               { ADD }
| '-'                                               { SUB }
| '*'                                               { TIMES }
| '/'                                               { DIV }
| '%'                                               { MOD }
| "+="                                              { ABBRADD }
| "-="                                              { ABBRSUB }
| "*="                                              { ABBRTIMES }
| "/="                                              { ABBRDIV }
| "%="                                              { ABBRMOD }
| "++"                                              { INCR }
| "--"                                              { DECR }
| '='                                               { ASSIGN }
| "=="                                              { EQ }
| "!="                                              { NEQ }
| '<'                                               { LT }
| "<="                                              { LEQ }
| '>'                                               { GT }
| ">="                                              { GEQ }
| "&&"                                              { AND }
| "||"                                              { OR }
| '!'                                               { NOT }
| '~'                                               { BITNOT }
| '^'                                               { BITXOR }
| '|'                                               { BITOR }
| "<<"                                              { SHIFTLEFT }
| ">>"                                              { SHIFTRIGHT }
| "&="                                              { ABBRBITAND }
| "|="                                              { ABBRBITOR }
| "^="                                              { ABBRBITXOR }
| "<<="                                             { ABBRBITSHL }
| ">>="                                             { ABBRBITSHR }
| '('                                               { LPAR }
| ')'                                               { RPAR }
| '{'                                               { LCBRACKET }
| '}'                                               { RCBRACKET }
| '['                                               { LSBRACKET }
| ']'                                               { RSBRACKET }
| ';'                                               { SEMICOL }
| ','                                               { COMMA }
| '.'                                               { DOT }
| "//"                                              { comment_single lexbuf }
| "/*"                                              { comment_multi lexbuf }
| eof                                               { EOF }
| _                                                 { raise_error lexbuf "Unexpected character"  }

and next_char = parse
| '\''                                              { raise_error lexbuf "Missing character" }
| '\\' (_ as sp) '\''                               { 
                                                        match (to_special sp) with
                                                        | Some spec -> VCHAR(spec)
                                                        | None      -> raise_error lexbuf "No such special character"
                                                    }
| [^'\''] as ch '\''                                { VCHAR(ch) }
| _                                                 { raise_error lexbuf ("No such character: " ^ (Lexing.lexeme lexbuf)) }

and next_string buf = parse
| '"'                                               { VSTRING(Buffer.contents buf) }
| '\\' (_ as sp)                                    {
                                                        (match (to_special sp) with
                                                        | Some spec -> Buffer.add_char buf spec
                                                        | None      -> raise_error lexbuf "No such special character");
                                                        next_string buf lexbuf 
                                                    }
| "\\0"                                             { Buffer.add_char buf (Char.chr 0); next_string buf lexbuf } (* The value of char 0 is \000 *)
| [^'"' '\\']+                                      { Buffer.add_string buf (Lexing.lexeme lexbuf); next_string buf lexbuf }
| eof                                               { raise_error lexbuf "String has not been terminated" }

and comment_single = parse
| '\n'                                              { Lexing.new_line lexbuf; next_token lexbuf }
| _                                                 { comment_single lexbuf }
| eof                                               { EOF }

and comment_multi= parse
| "*/"                                              { next_token lexbuf }
| '\n'												{ Lexing.new_line lexbuf; comment_multi lexbuf }
| _                                                 { comment_multi lexbuf }
| eof												{ raise_error lexbuf "EOF raised with open multiline comment" }