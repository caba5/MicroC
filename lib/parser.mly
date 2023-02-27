/*
* MicroC Parser specification
*/

%{
  (* Auxiliary definitions *)
  open Ast

  let new_node n_pos n_type = 
    { loc = Location.to_code_position(n_pos); node = n_type }
  
  (* Used for passing multiple data from vardecl -> allows multiple declarations *)
  type triple_vardec = {
    typf : typ -> typ;
    id : string;
    init : expr option;
  }

  (* Generates while's body placing the increment instruction, if present, at its end *)
  let generate_body incr stmt pos =
    match incr with
    | None    -> stmt
    | Some v  -> 
      let s_incr = new_node pos (Ast.Expr(v)) in
      new_node pos (Ast.Block([
        new_node pos (Ast.Stmt(stmt));
        new_node pos (Ast.Stmt(s_incr));
      ]))

  (* If no guard is specified a 'true' boolean literal is generated *)
  let generate_guard guard pos =
  match guard with 
    | None    -> new_node pos (Ast.BLiteral(true))
    | Some v  -> v
%}

/* Token declaration */

%token EOF
%token <string>ID
%token <int>VINT
%token <float>VFLOAT
%token <char>VCHAR
%token <string>VSTRING
%token <bool>VBOOL
%token IF RETURN ELSE FOR WHILE DO
%token INT FLOAT CHAR VOID NULL BOOL STRUCT
%token AMP "&" ADD "+" SUB "-" TIMES "*" DIV "/" MOD "%"
%token SIZEOF
%token ABBRADD "+=" ABBRSUB "-=" ABBRTIMES "*=" ABBRDIV "/=" ABBRMOD "%="
%token ABBRBITAND "&=" ABBRBITOR "|=" ABBRBITXOR "^=" ABBRBITSHL "<<=" ABBRBITSHR ">>="
%token INCR "++" DECR "--"
%token ASSIGN "=" EQ "==" NEQ "!=" LT "<" LEQ "<=" GT ">" GEQ ">=" AND "&&" OR "||" NOT "!" 
%token BITOR "|" BITXOR "^" BITNOT "~" SHIFTLEFT "<<" SHIFTRIGHT ">>"
%token LPAR "(" RPAR ")" LCBRACKET "{" RCBRACKET "}" LSBRACKET "[" RSBRACKET "]" SEMICOL ";" COMMA "," DOT "."                      

/* Precedence and associativity specification */
%nonassoc THEN  (* Introduced to overcome shift/reduce problem in if without else *)
%nonassoc ELSE  

%right    ASSIGN
%left     COMMA
%left     OR
%left     AND
%left     BITOR
%left     BITXOR
%nonassoc GT LT GEQ LEQ
%left     EQ NEQ
%left     ADD SUB
%left     TIMES DIV MOD
%left     ABBRADD ABBRSUB
%left     ABBRTIMES ABBRDIV ABBRMOD
%left     ABBRBITAND ABBRBITOR ABBRBITXOR ABBRBITSHL ABBRBITSHR
%right    SHIFTLEFT SHIFTRIGHT
%left     DOT (* used in 'lexpr' *)
%nonassoc NOT AMP
%nonassoc BITNOT
%nonassoc NEG (* used in 'rexpr' *)
%nonassoc LSBRACKET

/* Starting symbol */

%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */

%%

/* Grammar specification */

program:
  // Receives a list of lists from topdecl (because of multiple declarations),
  // hence the need to flatten (tail-recursive with 'concat')
  | td = list(topdecl) EOF   { Ast.Prog(List.concat td) }
;

topdecl:
  | t = typ multi_dec = separated_nonempty_list(",", vardecl) ";" 
    { 
      List.map (fun tup -> 
        let triple_vd = snd tup in 
        new_node (fst tup) (Ast.Vardec(triple_vd.typf t, triple_vd.id, triple_vd.init)) 
      ) multi_dec
    }
  | fd = fundecl      { [fd] }
  | sd = structdecl   { [sd] }
;

vardecl:
  | v = vardesc init = option(preceded("=", expr)) { ($loc, {typf = fst v; id = snd v; init = init}) }
;

// Introduced for fundecl and structdecl since they cannot use 'vardecl' due to initialization
vardecl_no_init:
  | t = typ v = vardesc { ((fst v) t, snd v) }
;

// Introduced for structdecl
// vardecl_member:
//   | t = typ v = vardesc init = option(preceded("=", expr)) { ((fst v) t, snd v, init) }
// ;

// Returns a tuple containing a function for mapping the declaration type 
// in the caller function (while also providing array or pointer types)
//  and the identifier of the declared variable
vardesc:
  | i = ID                                { ((fun t -> t), i) }
  | "*" vd = vardesc %prec AMP            { ((fun t -> fst vd (Ast.TypP(t))), snd vd) }
  | "(" vd = vardesc ")"                  { vd }
  | vd = vardesc "[" i = option(VINT) "]" { ((fun t -> fst vd (Ast.TypA(t,i))), snd vd) }
;

fundecl:
  // RE: "((vardecl ",")* vardecl)?" is the same as "(vardecl ",")?"
  | t = fun_typ i = ID "(" params = separated_list(",", vardecl_no_init) ")" b = block
    { 
      let body_block = new_node $loc b in
      new_node $loc (Ast.Fundecl{typ = t; fname = i; formals = params; body = body_block}) 
    }
;

structdecl:
  | STRUCT i = ID "{" membs = list(terminated(vardecl_no_init, ";")) "}" ";"
  {
    new_node $loc (Ast.StructDecl{sname = i; members = membs})
  }
;

block:
  // Receives a list of lists from stmt_or_dec (because of multiple declarations),
  // hence the need to flatten (tail-recursive with 'concat')
  | "{" b = list(stmt_or_dec) "}"   { Ast.Block(List.concat b) }
;

stmt_or_dec:
  | s = stmt { [new_node $loc (Ast.Stmt(s))] }
  | t = typ multi_dec = separated_nonempty_list(",", vardecl) ";" 
  {
    List.map (fun tup -> 
      let triple_vd = snd tup in
      new_node (fst tup) (Ast.Dec(triple_vd.typf t, triple_vd.id, triple_vd.init)) 
    ) multi_dec
  }
;

typ:
  | INT           { Ast.TypI } 
  | FLOAT         { Ast.TypF }
  | CHAR          { Ast.TypC }
  | BOOL          { Ast.TypB } 
  | STRUCT i = ID { Ast.TypS(i) }
;

// Introduced to allow defining void functions
%inline fun_typ:
  | VOID    { Ast.TypV } 
  | t = typ { t }
;

stmt:
  | RETURN e = option(expr_comma) ";"             { new_node $loc (Ast.Return(e)) }
  | e = expr_comma ";"                            { new_node $loc (Ast.Expr(e)) }
  | b = block                                     { new_node $loc b }
  | WHILE "(" e = expr ")" s = stmt               { new_node $loc (Ast.While(e, s)) }
  | DO s = stmt WHILE "(" e = expr ")" ";"        { new_node $loc (Ast.DoWhile(s, e)) }
  | FOR "(" init = option(expr_comma) ";" guard = option(expr_comma) ";" incr = option(expr_comma) ")" s = stmt
    { 
      let while_body = generate_body incr s $loc in
      let cond = generate_guard guard $loc in
      let while_node = new_node $loc (Ast.While(cond, while_body)) in
      match init with
      (* Returns the while node with no initialized value *)
      | None    -> while_node
      (* Prepends initialized value to the while node *)
      | Some v  -> 
      let s_init = new_node $loc (Ast.Expr(v)) in
      new_node $loc (Ast.Block([
        new_node $loc (Ast.Stmt(s_init));
        new_node $loc (Ast.Stmt(while_node));
      ]))
    }
  | FOR "(" init = for_init ";" guard = option(expr_comma) ";" incr = option(expr_comma) ")" s = stmt
    {
      let while_body = generate_body incr s $loc in
      let cond = generate_guard guard $loc in
      let while_node = new_node $loc (Ast.While(cond, while_body)) in
      (* Need to concat in since it can receive a list of variable initialization *)
      new_node $loc (Ast.Block(List.concat [
        init;
        [new_node $loc (Ast.Stmt(while_node))];
      ]))
    }
  | IF "(" cond = expr ")" true_stmt = stmt %prec THEN  // Moved up to avoid obfuscating the if without else prod.
    {
      let false_stmt = new_node $loc (Ast.Block([])) in
      new_node $loc (Ast.If(cond, true_stmt, false_stmt))
    }
  | IF "(" cond = expr ")" true_stmt = stmt ELSE false_stmt = stmt
    {  
      new_node $loc (Ast.If(cond, true_stmt, false_stmt))
    }
;

// Allows initializing variables in the FOR declaration
%inline for_init:
  | t = typ multi_dec = separated_nonempty_list(",", vardecl)
    { 
      List.map (fun tup -> 
        let triple_vd = snd tup in
        new_node (fst tup) (Ast.Dec(triple_vd.typf t, triple_vd.id, triple_vd.init)) 
      ) multi_dec
    }
;

// Used in conditionals and var declarations (with initialization)
expr:
  | re = rexpr                { new_node $loc re }
  | le = lexpr                { new_node $loc (Ast.Access(le)) }
;

// Used only in statements and allows using the comma operator
expr_comma:
  | re = rexpr_comma          { new_node $loc re }
  | le = lexpr                { new_node $loc (Ast.Access(le)) }
;

lexpr:  
  | i = ID                      { new_node $loc (Ast.AccVar(i)) }
  | "(" le = lexpr ")"          { le }
  | "*" le = lexpr              { 
                                  let access = new_node $loc (Ast.Access(le)) in
                                  new_node $loc (Ast.AccDeref(access))
                                }
  | "*" ae = aexpr              { new_node $loc (Ast.AccDeref(new_node $loc ae)) }
  | le = lexpr "[" e = expr "]" { new_node $loc (Ast.AccIndex(le, e)) }
  | le = lexpr "." member = ID  { new_node $loc (Ast.AccMember(le, member)) }
;

rexpr:
  | ae = aexpr                                        { ae }
  | i = ID "(" params = separated_list(",", expr) ")" { Ast.Call(i, params) }
  | le = lexpr "=" e = expr                           { Ast.Assign(le, e) }
  | "!" e = expr                                      { Ast.UnaryOp(Ast.Not, e) }
  | "-" e = expr %prec NEG                            { Ast.UnaryOp(Ast.Neg, e) }
  | SIZEOF e = expr %prec NEG                         { Ast.UnaryOp(Ast.Sizeof, e)}
  | "~" e = expr                                      { Ast.UnaryOp(Ast.BitNot, e) }
  | e1 = expr bo = binop e2 = expr                    { Ast.BinaryOp(bo, e1, e2) }
  | le = lexpr ab = abbr_binop e = expr 
    {
      (* Desugaring abbreviations *)
      let acc = new_node $loc (Ast.Access(le)) in
      let bop = new_node $loc (Ast.BinaryOp(ab, acc, e)) in
      Ast.Assign(le, bop)
    }
  | le = lexpr "++"                                   
    { 
      let acc = new_node $loc (Ast.Access(le)) in
      Ast.UnaryOp(Ast.PostInc, acc)
    }
  | le = lexpr "--"                                   
    {
      let acc = new_node $loc (Ast.Access(le)) in
      Ast.UnaryOp(Ast.PostDec, acc)
    }
  | "++" le = lexpr                                   
    {
      let acc = new_node $loc (Ast.Access(le)) in
      Ast.UnaryOp(Ast.PreInc, acc)
    }
  | "--" le = lexpr                                   
    {
      let acc = new_node $loc (Ast.Access(le)) in
      Ast.UnaryOp(Ast.PreDec, acc)
    }
;

// Used by expr_comma. Introduces the binary operator 'comma'
rexpr_comma:
  | e1 = expr_comma "," e2 = expr_comma { Ast.BinaryOp(Ast.Comma, e1, e2) }
  | re = rexpr                          { re }
;

%inline binop:  (* Need to inline to solve shift/reduce conflicts *)
  | "+"   { Ast.Add }
  | "-"   { Ast.Sub }
  | "*"   { Ast.Mult }
  | "%"   { Ast.Mod }
  | "/"   { Ast.Div }
  | "&&"  { Ast.And }
  | "||"  { Ast.Or }
  | "<"   { Ast.Less }
  | ">"   { Ast.Greater }
  | "<="  { Ast.Leq }
  | ">="  { Ast.Geq }
  | "=="  { Ast.Equal }
  | "!="  { Ast.Neq }
  | "&"   { Ast.BitAnd }
  | "|"   { Ast.BitOr }
  | "^"   { Ast.BitXor }
  | "<<"  { Ast.ShiftLeft }
  | ">>"  { Ast.ShiftRight }
;

%inline abbr_binop:
  | "+="  { Ast.Add }
  | "-="  { Ast.Sub }
  | "*="  { Ast.Mult }
  | "/="  { Ast.Div }
  | "%="  { Ast.Mod }
  | "&="  { Ast.BitAnd }
  | "|="  { Ast.BitOr }
  | "^="  { Ast.BitXor }
  | "<<=" { Ast.ShiftLeft }
  | ">>=" { Ast.ShiftRight }  
;

aexpr:
  | i = VINT                  { Ast.ILiteral(i) }
  | f = VFLOAT                { Ast.FLiteral(f) }
  | c = VCHAR                 { Ast.CLiteral(c) }
  | s = VSTRING               { Ast.SLiteral(s) }
  | b = VBOOL                 { Ast.BLiteral(b) }
  | NULL                      { Ast.Null }
  | "(" re = rexpr_comma ")"  { re }
  | "&" le = lexpr            { Ast.Addr(le) }
;