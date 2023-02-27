# MicroC Compiler

MicroC is a subset of the language C with the following simplification:

* It supports integers (`int`), floats (`float`), characters (`char`), and booleans (`bool`) as scalar values, array, pointers, strings, and structs (`struct`) as compound data types;
* No dynamic allocation of memory;
* Multi-dimensional arrays are supported;
* Variables can be initialized during their declarations;
* Supports pointer arithmetic;
* Pointers and arrays are interchangeable;
* Supports separate compilation (withot `#include` statements);
* there are only four library functions
```C
void print(int)     // it outputs an integer to standard output
void fprint(float)  // it outputs a float to standard output
void cprint(char)   // it outputs a char to standard output
int getint()        // it inputs an integer from standard input 
```

## MicroC Parser

The scanner is implemented in `ocamllex` whereas the parser is implemented in `menhir`.

## MicroC syntax
In the following there is the specification of the syntax of MicroC, which is a small variant of the syntax of the C language.

### Lexical elements

* Identifiers starts with a letter or an underscore and then can contain letters, underscore and numbers, e.g., `i`, `_local_var`, `string_of_int32`;

* Integer literals are sequences of digits in base 10 or digits in base 16 prefixed with `0x` (integers are 32bit values), e.g., `32`, `1024`, `3232`, `0xFF`, `0x10`;

* Float literals can be expressed either in normal form either in exponential form;

* Character literals have the form `'c'` where c is a character, e.g., `a`, `A`, `1`, special characters are `\'`,`\b`, `\f`, `\t`, `\\`, `\r`, and `\n`;

* Boolean literals are `true` and `false`;

* Keywords are: `if`, `return`, `else`, `for`, `while`, `int`, `char`, `void`, `NULL`, `bool`;

* Operators are: &, +, -, *, /, %, <<, >>, =, ==, !=, <, <=, >, >=, &&, ||, !, ++, --, ,, +=, -=, /=, *=, %=, <<=, >>=, sizeof

* Other symbols: (, ), {, }, [, ], &, ;

* Comments:
    * `//...` single line comments;
    * `/* ... */` multi line comments.

### Syntax

    Program ::= Topdecl * EOF
    Topdecl ::= Typ (( Vardecl " ,")* Vardecl )
            | Fundecl
            | StructDecl
    Vardecl ::= Vardesc ("=" Expr )?
    VardeclNoInit ::= Typ Vardesc
    Vardesc ::= ID
            | "*" Vardesc
            | "(" Vardesc ")"
            | Vardesc "[" INT ? "]"
    Fundecl ::= FunTyp ID "(" (( VardeclNoInit " ,")* VardeclNoInit )? ")" Block
    StructDecl ::= " struct " ID "{" ( VardeclNoInit ";")* "}" ";"
    Block ::= "{" StmtOrDec * "}"
    StmtOrDec ::= Stmt
            | Typ (( Vardecl " ,")* Vardecl ) ";"
    Typ ::= " int "
            | " float "
            | " char "
            | " bool "
            | " struct " ID
    FunTyp ::= " void "
            | Typ
    Stmt ::= " return " ExprComma ? ";"
            | ExprComma ";"
            | Block
            | " while " "(" Expr ")" Stmt
            | " do " Stmt " while " "(" Expr ")" ";"
            | " for " "(" ExprComma ? ";" ExprComma ? ";" ExprComma ? ")" Stmt
            | " for " "(" ForInit ";" ExprComma ? ";" ExprComma ? ")" Stmt
            | " if " "(" Expr ")" Stmt
            | " if " "(" Expr ")" Stmt " else " Stmt
    ForInit ::= Typ Vardecl
    Expr ::= RExpr
        | LExpr
    ExprComma ::= RExprComma
        | LExpr
    LExpr ::= ID
        | "(" LExpr ")"
        | "*" LExpr
        | "*" AExpr
        | LExpr "[" Expr "]"
        | LExpr "." ID
    RExpr ::= AExpr
        | ID "(" (( Expr ",")* Expr )? ")"
        | LExpr "=" Expr
        | "!" Expr
        | "-" Expr
        | "sizeof " Expr
        | "~" Expr
        | Expr BinOp Expr
        | LExpr AbbrBinOp Expr
        | LExpr "++"
        | LExpr " - -"
        | "++" LExpr
        | " - -" LExpr
    RExprComma ::= ExprComma "," ExprComma
        | RExpr
    BinOp::= "+"
        | "-"
        | "*"
        | "%"
        | "/"
        | "&&"
        | "||"
        | "<"
        | ">"
        | "<="
        | ">="
        | "=="
        | "!="
        | "&"
        | "|"
        | "^"
        | "<<"
        | ">>"
    AbbrBinOp::= "+="
        | "-="
        | "*="
        | "/="
        | "%="
        | "&="
        | "|="
        | "^="
        | "<<="
        |">>="
    AExpr ::= INT
        | FLOAT
        | CHAR
        | STRING
        | BOOL
        | NULL
        | "(" RExprComma ")"
        | "&" LExpr

## Result of the parsing
The parsing phase produces an AST whose node are annotated with a location in the code, if the program is syntactically correct, 
otherwise it raises an error.


