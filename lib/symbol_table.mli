exception DuplicateEntry of Ast.identifier

type 'a t

val empty_table : unit -> 'a t
val begin_block : 'a t -> unit
val end_block : 'a t -> unit
val add_entry : Ast.identifier -> 'a -> 'a t -> unit
val lookup : Ast.identifier -> 'a t -> 'a option
val of_alist : (Ast.identifier * 'a) list -> 'a t
val print : 'a t -> unit
