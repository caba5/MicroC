module L = Llvm
exception Codegen_error of Location.code_pos * string
val raise_error : Location.code_pos -> string -> 'a
type symbols = {
  fun_symtable : L.llvalue Symbol_table.t;
  var_symtable : L.llvalue Symbol_table.t;
  struct_symtable : (L.lltype * string list) Symbol_table.t;
}

(** Test  *)
val max_int_32 : int
val min_int_32 : int
val llcontext : L.llcontext
val int_t : L.lltype
val float_t : L.lltype
val bool_t : L.lltype
val char_t : L.lltype
val void_t : L.lltype
val zero : L.llvalue
val one : L.llvalue
val fone : L.llvalue
val add_rt_support : L.llmodule -> symbols -> unit
val get_simple_type : Ast.typ -> Location.code_pos -> symbols -> L.lltype
val build_and_store :
  L.llbuilder -> symbols -> L.lltype * string -> L.llvalue -> unit
val codegen_uop :
  L.llbuilder -> Ast.uop -> L.llvalue -> Location.code_pos -> L.llvalue
val codegen_inc_dec :
  L.llbuilder -> Ast.uop -> L.llvalue -> Location.code_pos -> L.llvalue
val codegen_bop :
  L.llbuilder ->
  Ast.binop -> L.lltype -> L.lltype -> L.llvalue -> L.llvalue -> L.llvalue
val codegen_expr :
  L.llbuilder -> symbols -> Ast.expr_node Ast.annotated_node -> L.llvalue
val codegen_access : L.llbuilder -> symbols -> Ast.access -> L.llvalue
val terminate_block : (L.llbuilder -> 'a) -> L.llbuilder -> unit
val codegen_stmt :
  L.llbuilder ->
  symbols -> Ast.stmt_node Ast.annotated_node -> L.llvalue -> unit
val codegen_stmtordec :
  L.llbuilder -> symbols -> Ast.stmtordec -> L.llvalue -> unit
val codegen_func : symbols -> Ast.fun_decl -> Location.code_pos -> unit
val codegen_bop_global :
  Ast.binop -> L.lltype -> L.lltype -> L.llvalue -> L.llvalue -> L.llvalue
val codegen_expr_global :
  Ast.expr_node Ast.annotated_node -> Ast.typ -> symbols -> L.llvalue
val codegen_global_var :
  L.llmodule ->
  symbols ->
  Location.code_pos ->
  Ast.typ * string -> Ast.expr_node Ast.annotated_node option -> unit
val codegen_topdecl : symbols -> Ast.topdecl_node Ast.annotated_node -> unit
val to_llvm_module : Ast.program -> L.llmodule
