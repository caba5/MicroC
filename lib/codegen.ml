open Ast
module L = Llvm

exception Codegen_error of Location.code_pos * string

let raise_error loc text = raise (Codegen_error (loc, text))

type symbols = {
  fun_symtable : L.llvalue Symbol_table.t;
  var_symtable : L.llvalue Symbol_table.t;
  (* struct_symtable contains the struct type and a series of member names *)
  struct_symtable : (L.lltype * string list) Symbol_table.t;
}

let max_int_32 = 0x7FFFFFFF
let min_int_32 = -max_int_32 - 1
let llcontext = L.global_context ()
let int_t = L.i32_type llcontext
let float_t = L.float_type llcontext
let bool_t = L.i1_type llcontext
let char_t = L.i8_type llcontext
let void_t = L.void_type llcontext
let zero = L.const_int int_t 0
let one = L.const_int int_t 1
let fone = L.const_float float_t 1.0

(** Declares the support functions from lib/rt-support.c *)
let add_rt_support llmodule gamma =
  let fun_print_t = L.function_type void_t [| int_t |] in
  let fun_print = L.declare_function "print" fun_print_t llmodule in
  Symbol_table.add_entry "print" fun_print gamma.fun_symtable;
  let fun_fprint_t = L.function_type void_t [| float_t |] in
  let fun_fprint = L.declare_function "fprint" fun_fprint_t llmodule in
  Symbol_table.add_entry "fprint" fun_fprint gamma.fun_symtable;
  let fun_cprint_t = L.function_type void_t [| char_t |] in
  let fun_cprint = L.declare_function "cprint" fun_cprint_t llmodule in
  Symbol_table.add_entry "cprint" fun_cprint gamma.fun_symtable;
  let fun_getint_t = L.function_type int_t [||] in
  let fun_getint = L.declare_function "getint" fun_getint_t llmodule in
  Symbol_table.add_entry "getint" fun_getint gamma.fun_symtable

(** Returns an LLVM type based on the Ast's type *)
let rec get_simple_type typ pos gamma =
  match typ with
  | Ast.TypI -> int_t
  | Ast.TypF -> float_t
  | Ast.TypB -> bool_t
  | Ast.TypC -> char_t
  | Ast.TypA (ty, Some si) -> L.array_type (get_simple_type ty pos gamma) si
  | Ast.TypA (ty, None) (* Llvm transforms arr parameter to ptr type *)
  | Ast.TypP ty ->
      get_simple_type ty pos gamma |> L.pointer_type
  | Ast.TypN -> L.pointer_type int_t
  | Ast.TypV -> void_t
  | Ast.TypS id -> (
      match Symbol_table.lookup id gamma.struct_symtable with
      | Some (t, _) -> t
      | None -> raise_error pos @@ "Undefined struct " ^ id)

(** Allocates space on the stack for the given variable and stores
   the parameter inside of it, also adding it to the symbol table *)
let build_and_store builder gamma (ty, id) par =
  (* let t = get_simple_type ty in *)
  let alloc = L.build_alloca ty "" builder in
  L.build_store par alloc builder |> ignore;
  Symbol_table.add_entry id alloc gamma.var_symtable

(** Creates unary operators *)
let codegen_uop builder uop exp loc =
  match uop with
  | Ast.Neg -> L.build_neg exp "neg" builder
  | Ast.Not -> L.build_not exp "not" builder
  | Ast.BitNot -> L.build_not exp "bitnot" builder
  | Ast.Sizeof ->
      let i64_sizeof = L.size_of (L.type_of exp) in
      (* Need to truncate from 64 bit integer to 32 bit *)
      L.build_trunc i64_sizeof int_t "" builder
  | _ -> raise_error loc "Invalid unitary operator"

(** Desugars pre/post increment/decrement and returns the correct value
   based on if the operation was "pre" or "post" *)
let codegen_inc_dec builder op acc loc =
  let pre = L.build_load acc "" builder in
  (* Need to pass the type into the 'element_type' function to get
     the type of arrays and pointers and avoid segmentation faults *)
  let post =
    match (op, L.type_of acc |> L.element_type) with
    | (Ast.PreInc | Ast.PostInc), t when t = int_t ->
        L.build_add pre one "" builder
    | (Ast.PreInc | Ast.PostInc), t when t = float_t ->
        L.build_fadd pre fone "" builder
    | (Ast.PreDec | Ast.PostDec), t when t = int_t ->
        L.build_sub pre one "" builder
    | (Ast.PreDec | Ast.PostDec), t when t = float_t ->
        L.build_fsub pre fone "" builder
    | _ -> raise_error loc "Invalid unitary operator"
  in
  L.build_store post acc builder |> ignore;
  if op = Ast.PreInc || op = Ast.PreDec then post else pre

(** Creates the right binary operators for the given operand types *)
let codegen_bop builder bop e1_t e2_t e1 e2 =
  match bop with
  | Ast.Add when e1_t = int_t && e2_t = int_t -> L.build_add e1 e2 "add" builder
  | Ast.Add when e1_t = float_t && e2_t = float_t ->
      L.build_fadd e1 e2 "add" builder
  | Ast.Sub when e1_t = int_t && e2_t = int_t -> L.build_sub e1 e2 "sub" builder
  | Ast.Sub when e1_t = float_t && e2_t = float_t ->
      L.build_fsub e1 e2 "sub" builder
  | Ast.Mult when e1_t = int_t && e2_t = int_t ->
      L.build_mul e1 e2 "mul" builder
  | Ast.Mult when e1_t = float_t && e2_t = float_t ->
      L.build_fmul e1 e2 "mul" builder
  | Ast.Div when e1_t = int_t && e2_t = int_t ->
      L.build_sdiv e1 e2 "div" builder
  | Ast.Div when e1_t = float_t && e2_t = float_t ->
      L.build_fdiv e1 e2 "div" builder
  | Ast.Mod when e1_t = int_t && e2_t = int_t ->
      L.build_srem e1 e2 "mod" builder
  | Ast.Mod when e1_t = float_t && e2_t = float_t ->
      L.build_frem e1 e2 "mod" builder
  | Ast.Equal when e1_t = int_t && e2_t = int_t ->
      L.build_icmp L.Icmp.Eq e1 e2 "eq" builder
  | Ast.Equal when e1_t = float_t && e2_t = float_t ->
      L.build_fcmp L.Fcmp.Oeq e1 e2 "eq" builder
  | Ast.Neq when e1_t = int_t && e2_t = int_t ->
      L.build_icmp L.Icmp.Ne e1 e2 "neq" builder
  | Ast.Neq when e1_t = float_t && e2_t = float_t ->
      L.build_fcmp L.Fcmp.One e1 e2 "neq" builder
  | Ast.Less when e1_t = int_t && e2_t = int_t ->
      L.build_icmp L.Icmp.Slt e1 e2 "lt" builder
  | Ast.Less when e1_t = float_t && e2_t = float_t ->
      L.build_fcmp L.Fcmp.Olt e1 e2 "lt" builder
  | Ast.Leq when e1_t = int_t && e2_t = int_t ->
      L.build_icmp L.Icmp.Sle e1 e2 "le" builder
  | Ast.Leq when e1_t = float_t && e2_t = float_t ->
      L.build_fcmp L.Fcmp.Ole e1 e2 "le" builder
  | Ast.Greater when e1_t = int_t && e2_t = int_t ->
      L.build_icmp L.Icmp.Sgt e1 e2 "gt" builder
  | Ast.Greater when e1_t = float_t && e2_t = float_t ->
      L.build_fcmp L.Fcmp.Ogt e1 e2 "gt" builder
  | Ast.Geq when e1_t = int_t && e2_t = int_t ->
      L.build_icmp L.Icmp.Sge e1 e2 "ge" builder
  | Ast.Geq when e1_t = float_t && e2_t = float_t ->
      L.build_fcmp L.Fcmp.Oge e1 e2 "ge" builder
  | Ast.And when e1_t = bool_t && e2_t = bool_t ->
      L.build_and e1 e2 "and" builder
  | Ast.Or when e1_t = bool_t && e2_t = bool_t -> L.build_or e1 e2 "or" builder
  | Ast.Equal when e1_t = bool_t && e2_t = bool_t ->
      L.build_icmp L.Icmp.Eq e1 e2 "eq" builder
  | Ast.Neq when e1_t = bool_t && e2_t = bool_t ->
      L.build_icmp L.Icmp.Ne e1 e2 "neq" builder
  | Ast.Equal when e1_t = char_t && e2_t = char_t ->
      L.build_icmp L.Icmp.Eq e1 e2 "eq" builder
  | Ast.Neq when e1_t = char_t && e2_t = char_t ->
      L.build_icmp L.Icmp.Ne e1 e2 "neq" builder
  | Ast.BitAnd when e1_t = int_t && e2_t = int_t ->
      L.build_and e1 e2 "bitand" builder
  | Ast.BitOr when e1_t = int_t && e2_t = int_t ->
      L.build_or e1 e2 "bitor" builder
  | Ast.BitXor when e1_t = int_t && e2_t = int_t ->
      L.build_xor e1 e2 "bitxor" builder
  | Ast.ShiftLeft when e1_t = int_t && e2_t = int_t ->
      L.build_shl e1 e2 "lshift" builder
  | Ast.ShiftRight when e1_t = int_t && e2_t = int_t ->
      L.build_lshr e1 e2 "rshift" builder (* No sign extension *)
  (* Since the type arguments are computed through 'type_of', in order to check if they are array
     types the function 'classify_type' is used and matched against the 'TypeKind' structure of LLVM *)
  | Ast.Neq
    when L.classify_type e1_t = L.TypeKind.Pointer
         && L.classify_type e2_t = L.TypeKind.Pointer ->
      L.build_icmp L.Icmp.Ne e1 e2 "neq" builder
  | Ast.Equal
    when L.classify_type e1_t = L.TypeKind.Pointer
         && L.classify_type e2_t = L.TypeKind.Pointer ->
      L.build_icmp L.Icmp.Eq e1 e2 "eq" builder
  (* Pointer arithmetic *)
  | Ast.Add when L.classify_type e1_t = L.TypeKind.Pointer && e2_t = int_t ->
      L.build_gep e1 [| e2 |] "" builder
  | _ ->
      raise_error Location.dummy_code_pos
      @@ "Type mismatch between operands " ^ L.string_of_lltype e1_t ^ " and "
      ^ L.string_of_lltype e1_t ^ "\n"

(** Generates expressions *)
let rec codegen_expr builder gamma expr =
  match expr.node with
  | Ast.Access a ->
      let addr = codegen_access builder gamma a in
      (* If trying to access an array variable, then don't build the load to allow GEP in caller *)
      if L.type_of addr |> L.element_type |> L.classify_type = L.TypeKind.Array
      then addr
      else L.build_load addr "load" builder
  | Ast.Assign (acc, e) ->
      let addr = codegen_access builder gamma acc in
      let ex = codegen_expr builder gamma e in
      if L.type_of ex |> L.element_type |> L.classify_type = L.TypeKind.Array
      then (
        let el = L.build_gep ex [| zero; zero |] "" builder in
        L.build_store el addr builder |> ignore;
        el)
      else (
        L.build_store ex addr builder |> ignore;
        ex
        (* Returning the expression allows assignment concatenation, i.e. x = y = 1 *))
  | Ast.Addr a -> codegen_access builder gamma a
  | Ast.ILiteral i ->
      if i > max_int_32 || i < min_int_32 then
        raise_error expr.loc "Integer overflow"
      else L.const_int int_t i
  | Ast.FLiteral f -> L.const_float float_t f
  | Ast.CLiteral c ->
      L.const_int char_t (Char.code c)
      (* Need ASCII code since char is managed as int *)
  | Ast.SLiteral s ->
      L.build_global_string (s ^ "\x00") ""
        builder (* A literal is considered global: e.g. "string" *)
  | Ast.BLiteral b -> (if b then 1 else 0) |> L.const_int bool_t
  | Ast.Null -> int_t |> L.pointer_type |> L.const_null
  | Ast.UnaryOp (uop, exp) -> (
      match uop with
      (* If the unary operator is a pre/post increment/decrement check that
         it is applied only to variables and not to literals *)
      | Ast.PreInc | Ast.PreDec | Ast.PostInc | Ast.PostDec -> (
          match exp.node with
          | Ast.Access acc ->
              let to_inc = codegen_access builder gamma acc in
              codegen_inc_dec builder uop to_inc exp.loc
          | _ ->
              raise_error expr.loc
                "Increment and decrement operators can only be applied to \
                 variables")
      | Ast.Neg | Ast.Not | Ast.BitNot | Ast.Sizeof ->
          let e = codegen_expr builder gamma exp in
          codegen_uop builder uop e expr.loc)
  | Ast.BinaryOp (bop, exp1, exp2) -> (
      let e1 = codegen_expr builder gamma exp1 in
      let e2 = codegen_expr builder gamma exp2 in
      match bop with
      (* Comma doesn't need to build any binary operator, just return the expr on the right *)
      | Ast.Comma -> e2
      | _ -> codegen_bop builder bop (L.type_of e1) (L.type_of e2) e1 e2)
  | Ast.Call (id, params) ->
      (* Getting the function declaration *)
      let f_dec =
        match Symbol_table.lookup id gamma.fun_symtable with
        | Some e -> e
        | None ->
            raise_error expr.loc @@ "Function " ^ id
            ^ " not defined in the current scope"
      in
      let formals = L.params f_dec |> Array.to_list in
      let actuals =
        List.map2
          (fun f a ->
            match a.node with
            (* Handles the case when the function is called
               passing variables as actual parameters *)
            | Ast.Access a -> (
                let a_val = codegen_access builder gamma a in
                let formal_t = f |> L.type_of |> L.classify_type in
                match formal_t with
                | L.TypeKind.Pointer -> (
                    match
                      a_val |> L.type_of |> L.element_type |> L.classify_type
                    with
                    (* If the formal is a pointer and the actual is an array
                       -> convert actual to pointer to first element of the array *)
                    | L.TypeKind.Array ->
                        L.build_gep a_val [| zero; zero |] "" builder
                    (* If the formal is a pointer and the actual is also a pointer
                       -> just load the value from the reference *)
                    | L.TypeKind.Pointer -> L.build_load a_val "" builder
                    (* If the formal is a pointer and the actual is neither a pointer nor
                       an array -> just return the llvalue from the variable *)
                    | _ -> a_val)
                (* If the formal is not a pointer, then check if the actual parameter is an array pointer *)
                | _ ->
                    if
                      a_val |> L.type_of |> L.element_type |> L.classify_type
                      = L.TypeKind.Array
                    then
                      let gep = L.build_gep a_val [| zero; zero |] "" builder in
                      L.build_load gep "" builder
                    else L.build_load a_val "" builder)
            | _ -> (
                (* If the actual parameter is a simple expression, check if it a string literal,
                   otherwise just return the expression's llvalue *)
                let e_val = codegen_expr builder gamma a in
                match L.type_of e_val |> L.element_type |> L.classify_type with
                (* If it is an array TypeKind, then it means that it is a string literal *)
                | L.TypeKind.Array ->
                    L.build_gep e_val [| zero; zero |] "" builder
                | _ -> e_val))
          formals params
      in
      (* No name to avoid errors on void funcs *)
      L.build_call f_dec (Array.of_list actuals) "" builder

(** Generates accesses *)
and codegen_access builder gamma acc =
  match acc.node with
  | Ast.AccVar var -> (
      match Symbol_table.lookup var gamma.var_symtable with
      | Some value -> value
      | None ->
          raise_error acc.loc @@ "Variable " ^ var
          ^ " not defined in the current scope")
  | Ast.AccDeref expr -> codegen_expr builder gamma expr
  | Ast.AccIndex (acc, exp) -> (
      let arr = codegen_access builder gamma acc in
      let ind = codegen_expr builder gamma exp in
      let arr_t = L.type_of arr in
      (* Need to correctly generate gep for pointers and arrays *)
      match L.classify_type arr_t with
      | L.TypeKind.Pointer
        when arr_t |> L.element_type |> L.classify_type = L.TypeKind.Array ->
          L.build_in_bounds_gep arr [| zero; ind |] "" builder
      | L.TypeKind.Pointer ->
          let load_val = L.build_load arr "" builder in
          L.build_in_bounds_gep load_val [| ind |] "" builder
      | _ -> L.build_in_bounds_gep arr [| zero; ind |] "" builder)
  | Ast.AccMember (acc, id) -> (
      (* Accessing a struct member *)
      let mem = codegen_access builder gamma acc in
      let s_name = mem |> L.type_of |> L.element_type |> L.struct_name in
      (* Since the struct is always named *)
      match Symbol_table.lookup (Option.get s_name) gamma.struct_symtable with
      | Some (_, members) -> (
          let ind_list = List.mapi (fun idx member -> (idx, member)) members in
          match
            List.find_map
              (fun (idx, member) -> if member = id then Some idx else None)
              ind_list
          with
          | Some idx -> L.build_struct_gep mem idx "" builder
          | None ->
              raise_error acc.loc @@ "No such member " ^ id ^ " in struct "
              ^ Option.get s_name)
      | None -> raise_error acc.loc @@ "No such struct " ^ Option.get s_name)

(** Avoids terminating blocks in the middle by building unconditional jumps
   and returns only if they are not already specified*)
let terminate_block terminator builder =
  match builder |> L.insertion_block |> L.block_terminator with
  | None -> terminator builder |> ignore
  | _ -> ()

(** Generates statements *)
let rec codegen_stmt builder gamma stmt dec_fun =
  match stmt.node with
  | Ast.If (e, stmt1, stmt2) ->
      (* Create blocks for then, else, and continuation branches*)
      let bthen = L.append_block llcontext "then" dec_fun in
      let belse = L.append_block llcontext "else" dec_fun in
      let bcont = L.append_block llcontext "cont" dec_fun in
      (* Creating two additional builders instead of changing position of the current one *)
      let tbuilder = L.builder_at_end llcontext bthen in
      let ebuilder = L.builder_at_end llcontext belse in
      let expr = codegen_expr builder gamma e in
      (* Conditional jump to the correct branch *)
      L.build_cond_br expr bthen belse builder |> ignore;
      (* Creation of branches' bodies *)
      codegen_stmt tbuilder gamma stmt1 dec_fun |> ignore;
      codegen_stmt ebuilder gamma stmt2 dec_fun |> ignore;
      (* Unconditional jumps in both true and false blocks to the continuation block *)
      terminate_block (L.build_br bcont) tbuilder;
      terminate_block (L.build_br bcont) ebuilder;
      L.position_at_end bcont builder
  | Ast.While (e, while_stmt) | Ast.DoWhile (while_stmt, e) ->
      (* Create blocks for the condition, body, and continuation*)
      let bcond = L.append_block llcontext "cond" dec_fun in
      let bbody = L.append_block llcontext "body" dec_fun in
      let bcont = L.append_block llcontext "cont" dec_fun in
      let cbuilder = L.builder_at_end llcontext bcond in
      let bbuilder = L.builder_at_end llcontext bbody in
      (* Unconditional jump from current to either the condition or the body
         depending if it is a while or dowhile loop *)
      (match stmt.node with
      | Ast.While _ -> terminate_block (L.build_br bcond) builder
      (* If it is not a While, it is a DoWhile  *)
      | _ -> terminate_block (L.build_br bbody) builder);
      let expr = codegen_expr cbuilder gamma e in
      (* Conditional jump from the condition to the while's body or to the continuation *)
      L.build_cond_br expr bbody bcont cbuilder |> ignore;
      codegen_stmt bbuilder gamma while_stmt dec_fun |> ignore;
      (* Unconditional jump from the end of the while's body to the condition *)
      terminate_block (L.build_br bcond) bbuilder;
      L.position_at_end bcont builder
  | Ast.Expr e -> codegen_expr builder gamma e |> ignore
  | Ast.Return e ->
      if Option.is_none e then terminate_block L.build_ret_void builder
      else
        let e_val = Option.get e |> codegen_expr builder gamma in
        terminate_block (L.build_ret e_val) builder
  | Ast.Block stmts_or_decs ->
      Symbol_table.begin_block gamma.var_symtable;
      List.iter
        (fun sod -> codegen_stmtordec builder gamma sod dec_fun)
        stmts_or_decs
      |> ignore;
      Symbol_table.end_block gamma.var_symtable

(** Generates blocks *)
and codegen_stmtordec builder gamma stmt dec_fun =
  match stmt.node with
  | Ast.Dec (t, i, None) ->
      let var = L.build_alloca (get_simple_type t stmt.loc gamma) i builder in
      Symbol_table.add_entry i var gamma.var_symtable
  | Ast.Dec (t, i, Some init) ->
      let var = L.build_alloca (get_simple_type t stmt.loc gamma) i builder in
      let init_val = codegen_expr builder gamma init in
      (* Check if the initialization value is a string and, if so, 
         no need to store it since it is already global *)
      if
        init_val |> L.type_of |> L.element_type |> L.classify_type
        = L.TypeKind.Array
      then Symbol_table.add_entry i init_val gamma.var_symtable
      else (
        L.build_store init_val var builder |> ignore;
        Symbol_table.add_entry i var gamma.var_symtable)
  | Ast.Stmt s -> codegen_stmt builder gamma s dec_fun

(** Generates function body *)
let codegen_func gamma f pos =
  let func =
    match Symbol_table.lookup f.fname gamma.fun_symtable with
    | Some e -> e
    | None -> raise_error pos @@ "No such function " ^ f.fname
  in
  Symbol_table.begin_block gamma.var_symtable;
  let ibuilder = L.builder_at_end llcontext (L.entry_block func) in
  let fparams_list = Array.to_list (L.params func) in
  (* Creates the space and allocates each formal argument *)
  List.iter2
    (fun (t, i) param ->
      let formal_t = get_simple_type t pos gamma in
      build_and_store ibuilder gamma (formal_t, i) param)
    f.formals fparams_list;
  codegen_stmt ibuilder gamma f.body func |> ignore;
  Symbol_table.end_block gamma.var_symtable;
  (* Build the correct return *)
  match f.typ with
  | Ast.TypV -> terminate_block L.build_ret_void ibuilder
  | _ ->
      let t = get_simple_type f.typ pos gamma |> L.undef in
      terminate_block (L.build_ret t) ibuilder

(** Generates binary operators using 'const', without builder (-> needed for globals) *)
let codegen_bop_global bop e1_t e2_t e1 e2 =
  match bop with
  | Ast.Add when e1_t = int_t && e2_t = int_t -> L.const_add e1 e2
  | Ast.Add when e1_t = float_t && e2_t = float_t -> L.const_fadd e1 e2
  | Ast.Sub when e1_t = int_t && e2_t = int_t -> L.const_sub e1 e2
  | Ast.Sub when e1_t = float_t && e2_t = float_t -> L.const_fsub e1 e2
  | Ast.Mult when e1_t = int_t && e2_t = int_t -> L.const_mul e1 e2
  | Ast.Mult when e1_t = float_t && e2_t = float_t -> L.const_fmul e1 e2
  | Ast.Div when e1_t = int_t && e2_t = int_t -> L.const_sdiv e1 e2
  | Ast.Div when e1_t = float_t && e2_t = float_t -> L.const_fdiv e1 e2
  | Ast.Equal when e1_t = int_t && e2_t = int_t -> L.const_icmp L.Icmp.Eq e1 e2
  | Ast.Equal when e1_t = float_t && e2_t = float_t ->
      L.const_fcmp L.Fcmp.Oeq e1 e2
  | Ast.Neq when e1_t = int_t && e2_t = int_t -> L.const_icmp L.Icmp.Ne e1 e2
  | Ast.Neq when e1_t = float_t && e2_t = float_t ->
      L.const_fcmp L.Fcmp.One e1 e2
  | Ast.Less when e1_t = int_t && e2_t = int_t -> L.const_icmp L.Icmp.Slt e1 e2
  | Ast.Less when e1_t = float_t && e2_t = float_t ->
      L.const_fcmp L.Fcmp.Olt e1 e2
  | Ast.Leq when e1_t = int_t && e2_t = int_t -> L.const_icmp L.Icmp.Sle e1 e2
  | Ast.Leq when e1_t = float_t && e2_t = float_t ->
      L.const_fcmp L.Fcmp.Ole e1 e2
  | Ast.Greater when e1_t = int_t && e2_t = int_t ->
      L.const_icmp L.Icmp.Sgt e1 e2
  | Ast.Greater when e1_t = float_t && e2_t = float_t ->
      L.const_fcmp L.Fcmp.Ogt e1 e2
  | Ast.Geq when e1_t = int_t && e2_t = int_t -> L.const_icmp L.Icmp.Sge e1 e2
  | Ast.Geq when e1_t = float_t && e2_t = float_t ->
      L.const_fcmp L.Fcmp.Oge e1 e2
  | Ast.And when e1_t = bool_t && e2_t = bool_t -> L.const_and e1 e2
  | Ast.Or when e1_t = bool_t && e2_t = bool_t -> L.const_or e1 e2
  | Ast.Equal when e1_t = bool_t && e2_t = bool_t ->
      L.const_icmp L.Icmp.Eq e1 e2
  | Ast.Neq when e1_t = bool_t && e2_t = bool_t -> L.const_icmp L.Icmp.Ne e1 e2
  | Ast.Neq
    when L.classify_type e1_t = L.TypeKind.Pointer
         && L.classify_type e2_t = L.TypeKind.Pointer ->
      L.const_icmp L.Icmp.Ne e1 e2
  | Ast.Equal
    when L.classify_type e1_t = L.TypeKind.Pointer
         && L.classify_type e2_t = L.TypeKind.Pointer ->
      L.const_icmp L.Icmp.Eq e1 e2
  | Ast.Neq when e1_t = char_t && e2_t = char_t -> L.const_icmp L.Icmp.Ne e1 e2
  | Ast.Equal when e1_t = char_t && e2_t = char_t ->
      L.const_icmp L.Icmp.Eq e1 e2
  | Ast.BitAnd when e1_t = int_t && e2_t = int_t -> L.const_and e1 e2
  | Ast.BitOr when e1_t = int_t && e2_t = int_t -> L.const_or e1 e2
  | Ast.BitXor when e1_t = int_t && e2_t = int_t -> L.const_xor e1 e2
  | Ast.ShiftLeft when e1_t = int_t && e2_t = int_t -> L.const_shl e1 e2
  | Ast.ShiftRight when e1_t = int_t && e2_t = int_t ->
      L.const_lshr e1 e2 (* No sign extension *)
  | _ ->
      raise_error Location.dummy_code_pos
      @@ "Type mismatch between operands " ^ L.string_of_lltype e1_t ^ " and "
      ^ L.string_of_lltype e1_t ^ "\n"

(** Helper function for generating global expressions without Access, Call, and Assign*)
let rec codegen_expr_global expr t gamma =
  match expr.node with
  | Ast.ILiteral i -> L.const_int int_t i
  | Ast.FLiteral f -> L.const_float float_t f
  | Ast.CLiteral c ->
      L.const_int char_t (Char.code c)
      (* Need ASCII code since char is managed as int *)
  | Ast.BLiteral b -> (if b then 1 else 0) |> L.const_int bool_t
  | Ast.SLiteral s -> L.const_string llcontext (s ^ "\000")
  | Ast.Null -> get_simple_type t expr.loc gamma |> L.const_null
  | Ast.UnaryOp (uop, exp) -> (
      let e = codegen_expr_global exp t gamma in
      match uop with
      | Ast.Neg -> L.const_neg e
      | Ast.Not | Ast.BitNot -> L.const_not e
      | Ast.Sizeof ->
          let i64_sizeof = L.size_of (L.type_of e) in
          (* Need to truncate from 64 bit integer to 32 bit *)
          L.const_trunc i64_sizeof int_t
      | _ -> raise_error exp.loc "Invalid operator for global variable")
  | Ast.BinaryOp (bop, exp1, exp2) ->
      let e1 = codegen_expr_global exp1 t gamma in
      let e2 = codegen_expr_global exp2 t gamma in
      codegen_bop_global bop (L.type_of e1) (L.type_of e2) e1 e2
  | _ -> raise_error expr.loc "Invalid initialization for global variable"

(** Generates global variables *)
let codegen_global_var llmodule gamma pos (t, i) init_option =
  let initial_val =
    match init_option with
    | None -> get_simple_type t pos gamma |> L.undef
    | Some init -> codegen_expr_global init t gamma
  in
  let var = L.define_global i initial_val llmodule in
  Symbol_table.add_entry i var gamma.var_symtable

let codegen_topdecl gamma node =
  match node.node with
  | Ast.Fundecl func -> codegen_func gamma func node.loc
  | Ast.Vardec _ -> ()
  | Ast.StructDecl _ -> ()

let to_llvm_module (Ast.Prog topdecls) =
  (* Helper function which immediately adds the global symbols (functions, variables, and structs)
     to their respective symbol tables in order to avoid not defined symbols *)
  let add_funcs_and_structs llmodule gamma node =
    match node.node with
    | Ast.Vardec (t, i, init_option) ->
        codegen_global_var llmodule gamma node.loc (t, i) init_option
    (* Add all function declarations to the symbol table, generating the body later *)
    | Ast.Fundecl f ->
        let ret_t = get_simple_type f.typ node.loc gamma in
        let formals_t =
          List.map (fun (t, _) -> get_simple_type t node.loc gamma) f.formals
        in
        let func_t = L.function_type ret_t (Array.of_list formals_t) in
        let func = L.define_function f.fname func_t llmodule in
        Symbol_table.add_entry f.fname func gamma.fun_symtable
    (* Add all struct definitions to the symbol table *)
    | Ast.StructDecl s ->
        let struc = L.named_struct_type llcontext s.sname in
        let ids = List.map snd s.members in
        Symbol_table.add_entry s.sname (struc, ids) gamma.struct_symtable;
        let types =
          List.map (fun m -> get_simple_type (fst m) node.loc gamma) s.members
        in
        L.struct_set_body struc (Array.of_list types) false
  in
  let gamma =
    {
      fun_symtable = Symbol_table.empty_table ();
      var_symtable = Symbol_table.empty_table ();
      struct_symtable = Symbol_table.empty_table ();
    }
  in
  let llmodule = L.create_module llcontext "microc-module" in
  add_rt_support llmodule gamma;
  List.iter (add_funcs_and_structs llmodule gamma) topdecls;
  List.iter (codegen_topdecl gamma) topdecls;
  llmodule
