# MicroC Compiler

MicroC is a subset of the language C with the following core functionalities:

* It supports integers (`int`), floats (`float`), characters (`char`), and booleans (`bool`) as scalar values, array, pointers, strings, and structs (`struct`) as compound data types;
* No dynamic allocation of memory;
* Multi-dimensional arrays are supported;
* NULL pointers are supported
* While, do-while, and for loops are supported.
* Variables can be initialized during their declarations;
* Supports pointer arithmetic;
* Pointers and arrays are interchangeable;
* Supports separate compilation (without `#include` statements);
* there are only four library functions
```C
void print(int)     // it outputs an integer to standard output
void fprint(float)  // it outputs a float to standard output
void cprint(char)   // it outputs a char to standard output
int getint()        // it inputs an integer from standard input 
```

## MicroC Parser

The scanner and the parser are implemented in `ocamllex` and `menhir`, respectively.

### MicroC syntax
In the following there is the specification of the syntax of MicroC, which is a small variant of the syntax of the C language.

### Lexical elements

* Identifiers starts with a letter or an underscore and then can contain letters, underscore and numbers, e.g., `i`, `_local_var`, `string_of_int32`;

* Integer literals are sequences of digits in base 10 or digits in base 16 prefixed with `0x` (integers are 32bit values), e.g., `32`, `1024`, `3232`, `0xFF`, `0x10`;

* Float literals can be expressed either in normal form either in exponential form;

* Character literals have the form `'c'` where c is a character, e.g., `a`, `A`, `1`, special characters are `\'`,`\b`, `\f`, `\t`, `\\`, `\r`, and `\n`;

* Boolean literals are `true` and `false`;

* Keywords are: `if`, `return`, `else`, `for`, `while`, `int`, `char`, `void`, `NULL`, `bool`;

* Operators are: &, +, -, *, /, %, <<, >>, =, ==, !=, <, <=, >, >=, &&, ||, , |, ^, !, ++, --, ,, +=, -=, /=, *=, %=, <<=, >>=, sizeof

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

### Result of the parsing
The parsing phase produces an AST whose node are annotated with a location in the code, if the program is syntactically correct, 
otherwise it raises an error.

## MicroC semantic analysis

### The static semantics of MicroC

MicroC mainly follows the same rules of the C language with some exceptions described below.

### Semantic rules
MicroC adopts a static scoping: 
* Block can be nested and the declaration of a variable `x` in a inner block hides possible declarations in outer blocks;
* There is a global scope that contains variables and function declarations;
* Functions cannot be nested;
* No function overloading.

The signature of the `main` can be:
* `int main()` when it returns an integer;
* `void main()` when it returns no value.

### Typing rules

There is no type coercion:
* booleans and characters cannot be converted to integers;
* arithmetic operators expect only equally-typed values;
* logical operators expect only boolean values;
* dereference operator expects a pointer;
* in `a[i]` we expect `a` to be an array and `i` to be an integer value;
* only functions can be invoked;
* a function call must provide a number of arguments equals to the parameters of the function;
* conditional guards in `if` and `while` statements expect boolean values.
* arrays should have a size of at least 1 element;
* arrays cannot be assigned, i.e., `array1 = array2` is not allowed;
* variables of type `void` are not allowed;
* function pointers are not supported;

### Result of the semantic analysis
The semantic analysis phase returns the AST of the program, if the program is well typed, otherwise it raises an error.

## MicroC code generation

The _codegen_ module transforms MicroC code into LLVM IR code.

### The static semantics of MicroC

MicroC mainly follows the same rules of the C language with some exceptions described below.

### Code generation & runtime support

The code generator uses OCaml's bindings for LLVM API.

The implementation of the library functions is written in C and resides in `bin/rt-support.c`.

### Result of the linking and code generation phases
The code generation phase produces a LLVM modules that can be compiled and statically linked with the code the runtime support. 

## Additional info

### Requirements to build the code
The code requires:
* OCaml >= 4.12.0
* Menhir >= 20210419
* ppx_deriving >= 5.2 
* llvm >= 12.0.0

The single dependencies can be installed via `opam`
```sh
$ opam install menhir ppx_deriving llvm
```
or by
```bash
make deps
```
to install all the dependencies.

### Building the code and running the tests
Typing `make` will generate a the compiler executable `bin/microcc.exe`, and the testing program `test/codegen_test.exe`:
```
$ make
```

To clean-up the folder, run:
```
$ make clean
```

To test a specific compilation step (parser/semantic_analysis/codegen) the files in the `test/samples` directory can be used, for example:
```
$ dune exec bin/microcc.exe -- samples/test-hello.mc
```
or 
```
$ dune exec test/codegen_test.exe -- samples/test-hello.mc
```

Two additional scripts are provided in order to test the single files:
* `test_helper.py`: tests the expected compilation result (i.e. success on files that should pass and error output on files that should fail) of all of the files in the `test/samples/` directory.
* `create_result.sh`: compiles the given file (or files, if separate compilation is used), executes the object code, and creates an output file containing the stdout of the execution.

For example, running `./create_result.sh test/samples/test-separatecomp1/*.mc` results in a new file `f1.out` in that same directory containing the stdout of the execution of the separately compiled `f1.mc` and `f2.mc` files.

### Test files

In the `test/samples` directory many test cases can be found: files starting with `fail-*` represent non-valid program whose compilation must abort with an error; 
files starting with `test-*` represent valid program whose execution must generate the output in the corresponding `*.out` file.

### The source code

The `lib/` directory contains the modules for each phase of the compiler. 

More precisely, the `lib/` directory provides:

    ast.ml                       <-- Definition of the abstract syntax tree of MicroC 
    microcc.ml                   <-- The file from which the executable is built
    location.ml                  <-- The module Location provides two data types to represent code locations
    location.mli                 <-- The interface of the module Location   
    parser.mly                   <-- Menhir specification of the grammar
    parsing.ml                   <-- The module Parsing implements the parser
    parsing.mli                  <-- The interface of the module Parsing  
    scanner.mll                  <-- ocamllex specification of the scanner 
    scanner.mli                  <-- The interface of the module Scanner
    symbol_table.ml              <-- The module Symbol_table provides the implementation of a symbol table
    symbol_table.mli             <-- The interface of the module Symbol_table
    semantic_analysis.ml         <-- The module Semantic_analysis implements the semantic checker
    semantic_analysis.mli        <-- The interface of the module Semantic_analysis
    codegen.ml                   <-- The module Codegen translates a µcomp-lang into a LLVM module
    codegen.mli                  <-- The interface of the module Codegen
    optimizer.ml                 <-- The module Optimizer runs a sequence of optimization on a LLVM module
    optimizer.mli                <-- The interface of the module Optimizer

The `bin/` directory provide:

    microcc.ml                   <-- The code of the compiler executable
    mcompc.mli                   <-- A dummy interface
    rt-support.c                 <-- A simple implementation of the functions of the standard library

### Running the compiler

The code for the binary of the compiler is in the `bin` directory.
This binary can be run by
```bash
make start source_file1.mc [source_file2.mc, ...]
```
Running the compiler with `make` does not allow to pass options to the binary. 
In this case, the binary can be invoked directly using `dune` with:
```bash 
dune exec bin/microcc.exe -- <options> source_file1.mc [source_file2.mc, ...]
```

### Project structure

The following snippet describes microc's project structure.

```text
.
├── bin/
|   Source for microc's compiler binary. This links to `Microc` library whose sources are defined in `lib/`.
│
├── lib/
|   Sources for Microc library implementing the different phases of compilation. 
│
├── test/
|   Tests different phases of the compiler.
│
├── dune-project
|   Dune file used to mark the root of the project and define project-wide parameters.
│
├── LICENSE
│
├── Makefile
|   Make file containing common development commands.
│
├── README.md
│
└── microc-lang.opam
    Opam package definition.
```

