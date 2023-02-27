open Ast

exception Semantic_error of Location.code_pos * string

let raise_error loc text = raise (Semantic_error (loc, text))

type symbols = {
  fun_symtable : Ast.fun_decl Symbol_table.t;
  var_symtable : Ast.typ Symbol_table.t;
  struct_symtable : Ast.struct_decl Symbol_table.t;
}

(** Utility function used for descriptive exceptions *)
let rec str_of_typ t =
  match t with
  | Ast.TypI -> "int"
  | Ast.TypF -> "float"
  | Ast.TypB -> "bool"
  | Ast.TypC -> "char"
  | Ast.TypA (ty, _) -> "arr of " ^ str_of_typ ty
  | Ast.TypP ty -> "pointer of " ^ str_of_typ ty
  | Ast.TypV -> "void"
  | Ast.TypS id -> "struct " ^ id
  | Ast.TypN -> "NULL"

(** Utility function used for descriptive exceptions *)
let str_of_bop t =
  match t with
  | Ast.Add -> "add"
  | Ast.Sub -> "sub"
  | Ast.Mult -> "mul"
  | Ast.Div -> "div"
  | Ast.Mod -> "mod"
  | Ast.Equal -> "eq"
  | Ast.Neq -> "neq"
  | Ast.Less -> "less"
  | Ast.Leq -> "leq"
  | Ast.Greater -> "greater"
  | Ast.Geq -> "geq"
  | Ast.And -> "and"
  | Ast.Or -> "or"
  | Ast.BitAnd -> "bitwise and"
  | Ast.BitOr -> "bitwise or"
  | Ast.BitXor -> "bitwise xor"
  | Ast.ShiftLeft -> "left shift"
  | Ast.ShiftRight -> "right shift"
  | Ast.Comma -> "comma"

(* let flip f x y = f y x *)

(** Checks if there is a main definition between the top-level declarations *)
let check_main global =
  let check_main_node node =
    match node.node with
    (* The main can be either void or int *)
    | Ast.Fundecl { typ = Ast.TypI; fname = "main"; formals = []; body = _ } ->
        true
    | Ast.Fundecl { typ = Ast.TypV; fname = "main"; formals = []; body = _ } ->
        true
    | Ast.Fundecl { typ = _; fname = "main"; formals = _; body = _ } ->
        raise_error node.loc "Invalid definition of 'main' function"
    | _ -> false
  in
  if List.exists check_main_node global then ()
  else raise_error Location.dummy_code_pos "Missing main definition"

(** Adds to the symbol table the declarations of the functions in bin/rt-support.c *)
let add_lib_func gamma =
  let lib_print =
    {
      typ = Ast.TypV;
      fname = "print";
      formals = [ (Ast.TypI, "n") ];
      body = { loc = Location.dummy_code_pos; node = Ast.Block [] };
    }
  in
  Symbol_table.add_entry "print" lib_print gamma.fun_symtable |> ignore;
  let lib_fprint =
    {
      typ = Ast.TypV;
      fname = "fprint";
      formals = [ (Ast.TypF, "n") ];
      body = { loc = Location.dummy_code_pos; node = Ast.Block [] };
    }
  in
  Symbol_table.add_entry "fprint" lib_fprint gamma.fun_symtable |> ignore;
  let lib_cprint =
    {
      typ = Ast.TypV;
      fname = "cprint";
      formals = [ (Ast.TypC, "c") ];
      body = { loc = Location.dummy_code_pos; node = Ast.Block [] };
    }
  in
  Symbol_table.add_entry "cprint" lib_cprint gamma.fun_symtable |> ignore;
  let lib_getint =
    {
      typ = Ast.TypI;
      fname = "getint";
      formals = [];
      body = { loc = Location.dummy_code_pos; node = Ast.Block [] };
    }
  in
  Symbol_table.add_entry "getint" lib_getint gamma.fun_symtable |> ignore

(** Checks the type declaration of multidimensional arrays *)
let rec check_inner_arr pos typ =
  match typ with
  | Ast.TypA (t, Some _) -> check_inner_arr pos t
  | Ast.TypA (_, None) ->
      raise_error pos
        "The declaration of multidimensional arrays must have bounds for all \
         dimensions except the first"
  | _ -> ()

(** Checks edge cases for array declaration as well as struct existence and pointer type conformity *)
let rec check_type pos typ gamma =
  match typ with
  (* | Ast.TypA (Ast.TypA (_,_), _)    -> raise_error pos "Multidimensional arrays are not permitted" *)
  | Ast.TypA (_, Some sz) when sz = 0 ->
      raise_error pos "Arrays must be of non-zero length"
  | Ast.TypA (_, Some sz) when sz < 0 ->
      raise_error pos "Array length cannot be negative"
  | Ast.TypA (t, _) -> check_inner_arr pos t
  | Ast.TypP t -> check_type pos t gamma
  | Ast.TypS id -> (
      match Symbol_table.lookup id gamma.struct_symtable with
      | Some _ -> ()
      | None -> raise_error pos @@ "Structure " ^ id ^ " is not defined")
  (* void pointers and arrays cannot be generated since they are implicitly checked by the parser *)
  | _ -> ()

(** Proxy function for 'check_type': also checks that the array has a specified dimension *)
let check_var_type pos var_typ gamma =
  match var_typ with
  | Ast.TypV ->
      raise_error pos "Cannot define void type variables"
      (* Should not be reached since it is implicitly checked by the parser *)
  | Ast.TypA (_, None) ->
      raise_error pos "Arrays must be declared with an initial size"
  | _ -> check_type pos var_typ gamma

(** Checks a variable type and adds it to the current scope in the variables symbol table *)
let check_var pos (typ, id) gamma =
  check_var_type pos typ gamma;
  try Symbol_table.add_entry id typ gamma.var_symtable
  with Symbol_table.DuplicateEntry id ->
    raise_error pos @@ "Variable " ^ id ^ " already defined"

(** Checks if the inner types of arrays and pointers match *)
let rec check_inner_type pos t1 t2 =
  match (t1, t2) with
  | Ast.TypA (a_t1, Some sz1), Ast.TypA (a_t2, Some sz2) ->
      if sz1 = sz2 then check_inner_type pos a_t1 a_t2
      else raise_error pos "Arrays must have the same size"
  | Ast.TypA (a_t1, None), Ast.TypA (a_t2, _)
  | Ast.TypA (a_t1, _), Ast.TypA (a_t2, None) ->
      check_inner_type pos a_t1 a_t2
  | Ast.TypP p_t1, Ast.TypP p_t2 -> check_inner_type pos p_t1 p_t2
  (* Pointers can be assigned null values *)
  | Ast.TypP _, Ast.TypN | Ast.TypN, Ast.TypP _ -> true
  (* Allows assigning an array to a pointer *)
  | Ast.TypP p_t, Ast.TypA (a_t, _) -> check_inner_type pos p_t a_t
  | typ1, typ2 -> typ1 = typ2

(** Returns the resulting type from the application of unary operators *)
let get_unaryexp_type pos uop exp_type =
  match (uop, exp_type) with
  | (Ast.Neg | Ast.PreInc | Ast.PreDec | Ast.PostInc | Ast.PostDec), t ->
      if t = Ast.TypI then Ast.TypI else Ast.TypF
  | Ast.Not, Ast.TypB -> Ast.TypB
  | Ast.BitNot, Ast.TypI -> Ast.TypI
  | Ast.Sizeof, _ -> Ast.TypI
  | _, t -> raise_error pos @@ "Operator not supported for type " ^ str_of_typ t

(** Returns the resulting type from the application of binary operators *)
let get_binaryexp_type pos bop exp1_type exp2_type =
  match (bop, exp1_type, exp2_type) with
  | (Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod), Ast.TypI, Ast.TypI ->
      Ast.TypI
  | ( (Ast.Equal | Ast.Neq | Ast.Less | Ast.Leq | Ast.Greater | Ast.Geq),
      Ast.TypI,
      Ast.TypI ) ->
      Ast.TypB
  | (Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod), Ast.TypF, Ast.TypF ->
      Ast.TypF
  | ( (Ast.Equal | Ast.Neq | Ast.Less | Ast.Leq | Ast.Greater | Ast.Geq),
      Ast.TypF,
      Ast.TypF ) ->
      Ast.TypB
  | (Ast.And | Ast.Or | Ast.Equal | Ast.Neq), Ast.TypB, Ast.TypB -> Ast.TypB
  | ( (Ast.BitAnd | Ast.BitOr | Ast.BitXor | Ast.ShiftLeft | Ast.ShiftRight),
      Ast.TypI,
      Ast.TypI ) ->
      Ast.TypI
  | (Ast.Equal | Ast.Neq), (Ast.TypP _ | Ast.TypN), (Ast.TypP _ | Ast.TypN) ->
      Ast.TypB
  (* Allows pointer arithmetic *)
  | (Ast.Add | Ast.Sub), Ast.TypP t, Ast.TypI -> Ast.TypP t
  | Ast.Comma, _, t -> t
  | _ ->
      raise_error pos @@ "Operator " ^ str_of_bop bop
      ^ " not supported between type " ^ str_of_typ exp1_type ^ " and "
      ^ str_of_typ exp2_type

(** Checks the assignment of a string literal to an array of chars
   -> if array's length is not defined -> assign string
   -> if array's length is defined -> check that the string fits *)
let check_str_init pos arr_len id s gamma =
  try
    let str_len = String.length s + 1 in
    (* Length with \0 terminator *)
    if arr_len = 0 then
      Symbol_table.add_entry id
        (Ast.TypA (Ast.TypC, Some str_len))
        gamma.var_symtable
    else if arr_len >= str_len then
      Symbol_table.add_entry id
        (Ast.TypA (Ast.TypC, Some arr_len))
        gamma.var_symtable
    else raise_error pos "String has greater length than array's definition"
  with Symbol_table.DuplicateEntry i ->
    raise_error pos @@ "Variable " ^ i ^ " already defined"

(** Returns the type of an expression *)
let rec get_expr_type gamma expr =
  match expr.node with
  | Ast.Access a -> get_access_type a gamma
  | Ast.Assign (acc, e) -> (
      let acc_type = get_access_type acc gamma in
      match acc_type with
      | Ast.TypA (_, _) -> raise_error e.loc "Cannot assign to array"
      | _ ->
          let expr_type = get_expr_type gamma e in
          (* Check the conformity of the assignment, i.e. if the types match *)
          if check_inner_type expr.loc acc_type expr_type then acc_type
          else
            raise_error e.loc @@ "Cannot assign a value of type "
            ^ str_of_typ expr_type ^ " to a value of type "
            ^ str_of_typ acc_type)
  | Ast.Addr a -> Ast.TypP (get_access_type a gamma)
  | Ast.ILiteral _ -> Ast.TypI
  | Ast.FLiteral _ -> Ast.TypF
  | Ast.CLiteral _ -> Ast.TypC
  (* The string is transformed to an array of chars terminated by \0 *)
  | Ast.SLiteral s -> Ast.TypA (Ast.TypC, Some (String.length s + 1))
  | Ast.BLiteral _ -> Ast.TypB
  | Ast.Null -> Ast.TypN
  | Ast.UnaryOp (uop, exp) ->
      let exp_type = get_expr_type gamma exp in
      get_unaryexp_type expr.loc uop exp_type
  | Ast.BinaryOp (bop, exp1, exp2) ->
      let exp1_type = get_expr_type gamma exp1 in
      let exp2_type = get_expr_type gamma exp2 in
      get_binaryexp_type expr.loc bop exp1_type exp2_type
  | Ast.Call (id, params) -> (
      (* Check the expression type of each actual parameter *)
      let params_types = List.map (get_expr_type gamma) params in
      match Symbol_table.lookup id gamma.fun_symtable with
      (* If function exists *)
      | Some e ->
          let formals_types = List.map (fun (typ, _) -> typ) e.formals in
          let len_params = List.length params_types in
          let len_formals = List.length formals_types in
          if len_params < len_formals then
            raise_error expr.loc
              "Missing one or more arguments in function call"
          else if len_params > len_formals then
            raise_error expr.loc "Too many arguments passed in function call"
          else (
            (* If the correct amount of arguments is passed *)
            List.iter2
              (fun par_t form_t ->
                (* If the actual params have the same types of the formals *)
                if check_inner_type expr.loc par_t form_t then ()
                else
                  raise_error expr.loc @@ "Parameter type mismatch: function "
                  ^ e.fname ^ " expected " ^ str_of_typ form_t ^ " but got "
                  ^ str_of_typ par_t)
              params_types formals_types;
            e.typ)
      | None -> raise_error expr.loc @@ "Function " ^ id ^ " is not defined")

(** Returns the type of an access (variable/reference/array element/struct member) *)
and get_access_type access_node gamma =
  match access_node.node with
  | Ast.AccVar var -> (
      match Symbol_table.lookup var gamma.var_symtable with
      | Some value -> value
      | None ->
          raise_error access_node.loc
          @@ "Variable " ^ var ^ " not defined in the current scope")
  | Ast.AccDeref expr -> (
      let pointer_type = get_expr_type gamma expr in
      match pointer_type with
      | Ast.TypP t -> t
      | _ -> raise_error access_node.loc "Not a pointer")
  | Ast.AccIndex (acc, exp) -> (
      let index_type = get_expr_type gamma exp in
      match index_type with
      | Ast.TypI -> (
          let acc_type = get_access_type acc gamma in
          match acc_type with
          | Ast.TypA (typ, _) -> typ
          | _ -> raise_error access_node.loc "Accessing a non-array")
      | _ -> raise_error access_node.loc "Indices have to be integers")
  | Ast.AccMember (acc, id) -> (
      (* First check the variable having struct type, then check the struct member *)
      let acc_type = get_access_type acc gamma in
      match acc_type with
      | Ast.TypS s -> (
          match Symbol_table.lookup s gamma.struct_symtable with
          | Some struc -> (
              match
                List.find_opt (fun (_, mem_name) -> mem_name = id) struc.members
              with
              | Some (t, _) -> t
              | None ->
                  raise_error access_node.loc
                  @@ "No such member " ^ id ^ " in struct " ^ struc.sname)
          | None -> raise_error access_node.loc @@ "No such struct " ^ s)
      | _ -> raise_error access_node.loc "Trying to access non-struct variable")

(** Checks statements' internal constructs *)
let rec check_stmt typ body gamma is_first_block =
  match body.node with
  | Ast.If (e, stmt1, stmt2) ->
      if get_expr_type gamma e = Ast.TypB then (
        check_stmt typ stmt1 gamma is_first_block |> ignore;
        check_stmt typ stmt2 gamma is_first_block |> ignore)
      else raise_error body.loc "The guard has to be of boolean type"
  | Ast.While (e, stmt) | Ast.DoWhile (stmt, e) ->
      if get_expr_type gamma e = Ast.TypB then
        check_stmt typ stmt gamma is_first_block |> ignore
      else raise_error body.loc "The guard has to be of boolean type"
  | Ast.Expr e -> get_expr_type gamma e |> ignore
  | Ast.Return (Some e) ->
      (* Check if the returned type matches the function type *)
      if get_expr_type gamma e != typ then
        raise_error body.loc "The returned value does not match the return type"
      else ()
  | Ast.Return None ->
      (* A non-void function must always return a value *)
      if typ = Ast.TypV then ()
      else raise_error body.loc "Missing return statement"
  | Ast.Block stmts_or_decs ->
      (* Since the table's block representing the function body has already
         been opened by the caller we have to avoid opening a duplicate one,
         allowing us to check for possible function parameters redefinition *)
      if is_first_block then List.iter (check_stmtordec gamma typ) stmts_or_decs
      else (
        Symbol_table.begin_block gamma.var_symtable |> ignore;
        List.iter (check_stmtordec gamma typ) stmts_or_decs;
        Symbol_table.end_block gamma.var_symtable)

(** Checks individual blocks *)
and check_stmtordec gamma typ stmt =
  match stmt.node with
  | Ast.Dec (t, i, None) -> check_var stmt.loc (t, i) gamma
  | Ast.Dec (t, i, Some init) -> (
      (* Declaration with initialization *)
      match (t, init.node) with
      (* If string initialization *)
      | Ast.TypA (Ast.TypC, None), Ast.SLiteral s ->
          check_str_init stmt.loc 0 i s gamma
      | Ast.TypA (Ast.TypC, Some sz), Ast.SLiteral s ->
          check_str_init stmt.loc sz i s gamma
      (* Otherwise *)
      | _ ->
          check_var stmt.loc (t, i) gamma;
          let init_typ = get_expr_type gamma init in
          if check_inner_type stmt.loc t init_typ then ()
          else
            raise_error stmt.loc @@ "Cannot assign type " ^ str_of_typ init_typ
            ^ " to a type " ^ str_of_typ t)
  | Ast.Stmt s ->
      check_stmt typ s gamma
        false (* If here, the next block can't be the first, so pass false *)

(** Checks if there is some unreachable code after an always reached return *)
let rec check_dead_code block =
  match block.node with
  | Ast.If (_, stmt1, stmt2) ->
      (* Check if a return is present in the middle of a conditional branch *)
      check_dead_code stmt1 |> ignore;
      check_dead_code stmt2 |> ignore;
      false
  | Ast.While (_, stmt) | Ast.DoWhile (stmt, _) ->
      (* Check if a return is present in the middle of a (do-)while loop *)
      check_dead_code stmt |> ignore;
      false
  | Ast.Expr _ -> false
  | Ast.Return _ -> true
  | Ast.Block stmts ->
      (* Raises an error as soon as a return is found in the middle of the block  *)
      let f = List.map check_dead_stmtordec stmts in
      List.fold_left
        (fun curr elem_has_ret ->
          if curr then raise_error block.loc "Code found after return statement"
          else elem_has_ret)
        false f

(** Checks if a return statement is found in the middle of a block *)
and check_dead_stmtordec sod =
  match sod.node with Ast.Dec _ -> false | Ast.Stmt s -> check_dead_code s

(** Checks if the declarations of a function's parameters are legal *)
let check_par gamma pos (typ, id) =
  (* Function params can contain unsized arrays *)
  match typ with
  | Ast.TypV ->
      raise_error pos
        "Illegal parameter of type void" (* Should not get here due to parser *)
  | _ -> (
      (* Calls directly check_type since it can declare unsized arrays *)
      check_type pos typ gamma;
      try Symbol_table.add_entry id typ gamma.var_symtable |> ignore
      with Symbol_table.DuplicateEntry n ->
        raise_error pos @@ "Parameter " ^ n
        ^ " already defined in the current scope")

(** Checks the return type, the parameters, and the body of a function *)
let check_func pos func gamma =
  match func.typ with
  | Ast.TypA (_, _) | Ast.TypP _ ->
      raise_error pos "Illegal function return type"
  | _ ->
      Symbol_table.begin_block gamma.var_symtable;
      List.iter (check_par gamma pos) func.formals;
      check_dead_code func.body |> ignore;
      check_stmt func.typ func.body gamma true;
      Symbol_table.end_block gamma.var_symtable

(** Checks that a struct has no duplicated members *)
let check_struct pos s gamma =
  Symbol_table.begin_block gamma.var_symtable;
  List.iter (fun (typ, id) -> check_var pos (typ, id) gamma) s.members;
  Symbol_table.end_block gamma.var_symtable

(** Checks function and struct declarations *)
let check_topdecl gamma decl =
  match decl.node with
  | Ast.Fundecl func -> check_func decl.loc func gamma
  | Ast.StructDecl s -> check_struct decl.loc s gamma
  | Ast.Vardec (_, _, _) -> ()

let type_check (Ast.Prog topdecls) =
  (* Function used to directly add to the symbol table all of the global symbols
     avoiding errors related to unknown declarations in the symbol table *)
  let add_top_node gamma node =
    match node.node with
    | Ast.Vardec (typ, id, None) -> check_var node.loc (typ, id) gamma
    | Ast.Vardec (typ, id, Some init) -> (
        match (typ, init.node) with
        | Ast.TypA (Ast.TypC, None), Ast.SLiteral s ->
            check_str_init node.loc 0 id s gamma
        | Ast.TypA (Ast.TypC, Some sz), Ast.SLiteral s ->
            check_str_init node.loc sz id s gamma
        | _ -> (
            check_var node.loc (typ, id) gamma;
            let init_typ = get_expr_type gamma init in
            match init_typ with
            | Ast.TypA _ -> raise_error node.loc "Cannot assign an array type"
            | _ ->
                if check_inner_type node.loc typ init_typ then ()
                else
                  raise_error node.loc @@ "Cannot assign type "
                  ^ str_of_typ init_typ ^ " to a type " ^ str_of_typ typ))
    | Ast.Fundecl f -> (
        try Symbol_table.add_entry f.fname f gamma.fun_symtable
        with Symbol_table.DuplicateEntry id ->
          raise_error node.loc @@ "Function " ^ id ^ " already defined")
    | Ast.StructDecl s -> (
        try Symbol_table.add_entry s.sname s gamma.struct_symtable
        with Symbol_table.DuplicateEntry id ->
          raise_error node.loc @@ "Struct " ^ id ^ " already defined")
  in
  let gamma =
    {
      fun_symtable = Symbol_table.empty_table ();
      var_symtable = Symbol_table.empty_table ();
      struct_symtable = Symbol_table.empty_table ();
    }
  in
  check_main topdecls;
  add_lib_func gamma;
  List.iter (add_top_node gamma) topdecls;
  List.iter (check_topdecl gamma) topdecls;
  Ast.Prog topdecls
