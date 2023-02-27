type binop =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
  | BitAnd
  | BitOr
  | BitXor
  | ShiftLeft
  | ShiftRight
  | Comma
[@@deriving show]

type uop = Neg | Not | BitNot | PreInc | PostInc | PreDec | PostDec | Sizeof
[@@deriving show]

type identifier = string [@@deriving show]

type 'a annotated_node = { loc : Location.code_pos; [@opaque] node : 'a }
[@@deriving show]

type typ =
  | TypI (* Type int *)
  | TypF (* Type float *)
  | TypB (* Type bool *)
  | TypC (* Type char *)
  | TypA of typ * int option (* Array type *)
  | TypP of typ (* Pointer type  *)
  | TypV (* Type void  *)
  | TypS of identifier (* Struct type *)
  | TypN (* Type null *)
[@@deriving show]

and expr = expr_node annotated_node

and expr_node =
  | Access of access (* x  or  *p  or  a[e]  *)
  | Assign of access * expr (* x=e  or  *p=e  or  a[e]=e   *)
  | Addr of access (* &x   or  &*p   or  &a[e]  *)
  | ILiteral of int (* Integer literal  *)
  | FLiteral of float (* Float literal  *)
  | CLiteral of char (* Char literal    *)
  | SLiteral of string (* String literal *)
  | BLiteral of bool (* Bool literal    *)
  | Null
  | UnaryOp of uop * expr (* Unary primitive operator  *)
  | BinaryOp of binop * expr * expr (* Binary primitive operator  *)
  | Call of identifier * expr list (* Function call f(...)    *)
[@@deriving show]

and access = access_node annotated_node

and access_node =
  | AccVar of identifier (* Variable access    x  *)
  | AccDeref of expr (* Pointer dereferencing  *p *)
  | AccIndex of access * expr (* Array indexing   a[e] *)
  | AccMember of access * identifier (* Struct member access *)
[@@deriving show]

and stmt = stmt_node annotated_node

and stmt_node =
  | If of expr * stmt * stmt (* Conditional    *)
  | While of expr * stmt (* While loop     *)
  | DoWhile of stmt * expr (* Do-while loop   *)
  | Expr of expr (* Expression statement   e;  *)
  | Return of expr option (* Return statement  *)
  | Block of stmtordec list (* Block: grouping and scope *)
[@@deriving show]

and stmtordec = stmtordec_node annotated_node

and stmtordec_node =
  | Dec of typ * identifier * expr option
    (* Local variable declaration. Added expr for initializing var *)
  | Stmt of stmt (* A statement   *)
[@@deriving show]

type fun_decl = {
  typ : typ;
  fname : string;
  formals : (typ * identifier) list;
  body : stmt;
}
[@@deriving show]

type struct_decl = {
  sname : string;
  (* members : (typ * identifier * expr option) list; *)
  members : (typ * identifier) list;
}
[@@deriving show]

type topdecl = topdecl_node annotated_node

and topdecl_node =
  | Fundecl of fun_decl
  | Vardec of
      typ * identifier * expr option (* Added expr for initializing variable *)
  | StructDecl of struct_decl
[@@deriving show]

type program = Prog of topdecl list [@@deriving show]
