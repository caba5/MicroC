(** Returns a flattened version of the parsed modules *)
let link_progs progs =
  let topdecls_comb = List.concat_map (fun (Ast.Prog td) -> td) progs in
  Ast.Prog topdecls_comb
