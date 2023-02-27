exception Semantic_error of Location.code_pos * string
val raise_error : Location.code_pos -> string -> 'a
type symbols = {
  fun_symtable : Ast.fun_decl Symbol_table.t;
  var_symtable : Ast.typ Symbol_table.t;
  struct_symtable : Ast.struct_decl Symbol_table.t;
}
val str_of_typ : Ast.typ -> string
val str_of_bop : Ast.binop -> string
val check_main : Ast.topdecl_node Ast.annotated_node list -> unit
val add_lib_func : symbols -> unit
val check_inner_arr : Location.code_pos -> Ast.typ -> unit
val check_type : Location.code_pos -> Ast.typ -> symbols -> unit
val check_var_type : Location.code_pos -> Ast.typ -> symbols -> unit
val check_var : Location.code_pos -> Ast.typ * string -> symbols -> unit
val check_inner_type : Location.code_pos -> Ast.typ -> Ast.typ -> bool
val get_unaryexp_type : Location.code_pos -> Ast.uop -> Ast.typ -> Ast.typ
val get_binaryexp_type :
  Location.code_pos -> Ast.binop -> Ast.typ -> Ast.typ -> Ast.typ
val check_str_init :
  Location.code_pos -> int -> string -> string -> symbols -> unit
val get_expr_type : symbols -> Ast.expr_node Ast.annotated_node -> Ast.typ
val get_access_type : Ast.access -> symbols -> Ast.typ
val check_stmt :
  Ast.typ -> Ast.stmt_node Ast.annotated_node -> symbols -> bool -> unit
val check_stmtordec : symbols -> Ast.typ -> Ast.stmtordec -> unit
val check_dead_code : Ast.stmt_node Ast.annotated_node -> bool
val check_dead_stmtordec : Ast.stmtordec -> bool
val check_par : symbols -> Location.code_pos -> Ast.typ * string -> unit
val check_func : Location.code_pos -> Ast.fun_decl -> symbols -> unit
val check_struct : Location.code_pos -> Ast.struct_decl -> symbols -> unit
val check_topdecl : symbols -> Ast.topdecl_node Ast.annotated_node -> unit
val type_check : Ast.program -> Ast.program
