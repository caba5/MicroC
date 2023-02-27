exception DuplicateEntry of Ast.identifier

(* Storing only one table since adding a key that already exists
   just hides the previous one. This works well for defining blocks,
   as long as we keep track of variables defined in the current block *)
type 'a t = {
  table : (Ast.identifier, 'a * int) Hashtbl.t;
  mutable block_level : int;
  mutable curr_level_ids : Ast.identifier list list;
}

let empty_table () =
  { table = Hashtbl.create 0; block_level = 0; curr_level_ids = [] }

let begin_block t =
  t.block_level <- t.block_level + 1;
  t.curr_level_ids <- [] :: t.curr_level_ids

let end_block t =
  if t.block_level > 0 then (
    t.block_level <- t.block_level - 1;
    if List.length t.curr_level_ids > 0 then (
      List.iter (Hashtbl.remove t.table) (List.hd t.curr_level_ids);
      t.curr_level_ids <- List.tl t.curr_level_ids)
    else ())
  else failwith "Trying to end a block while there are no open blocks"

let add_entry key value t =
  (match Hashtbl.find_opt t.table key with
  | None -> ()
  | Some (_, lvl) when lvl = t.block_level -> raise (DuplicateEntry key)
  | _ -> ());
  Hashtbl.add t.table key (value, t.block_level);
  let new_cl_ids =
    if List.length t.curr_level_ids > 0 then
      (key :: List.hd t.curr_level_ids) :: List.tl t.curr_level_ids
    else [ key ] :: t.curr_level_ids
  in
  t.curr_level_ids <- new_cl_ids

let lookup key t =
  match Hashtbl.find_opt t.table key with None -> None | Some (v, _) -> Some v

let of_alist l =
  let newTable = empty_table () in
  List.iter (fun (k, v) -> add_entry k v newTable) l;
  newTable

let print t =
  let string_of_ids ids =
    List.fold_left (fun acc id -> acc ^ id ^ ", ") "" ids
  in
  Printf.printf "\nBlock: %d, ids: %s" t.block_level
    (string_of_ids (List.hd t.curr_level_ids))
