open Microc

type action = Parse | Type_check | Dump_llvm_ir | Compile

let[@inline] ( >> ) f g x = g (f x)

let load_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

(* Helper function which parses every passed module *)
let load_and_parse f =
  let source = load_file f in
  let lexbuf = Lexing.from_string ~with_positions:true source in
  Parsing.parse f lexbuf

let action_function outputfile optimize verify_module = function
  | Parse ->
      List.iter (fun file_name ->
          file_name |> load_and_parse |> Ast.show_program
          |> Printf.printf "Parsing succeded for file%s\n\n%s\n" file_name)
  | Type_check ->
      List.map (fun file_name -> file_name |> load_and_parse)
      >> Linker.link_progs >> Semantic_analysis.type_check >> Ast.show_program
      >> Printf.printf "Type-check succeded!\n\n%s\n"
  | Dump_llvm_ir ->
      List.map (fun file_name -> file_name |> load_and_parse)
      >> Linker.link_progs >> Semantic_analysis.type_check
      >> Codegen.to_llvm_module
      >> (fun llmodule ->
           if verify_module then Llvm_analysis.assert_valid_module llmodule;
           llmodule)
      >> (if optimize then Optimizer.optimize_module else Fun.id)
      >> Llvm.dump_module
  | Compile ->
      (List.map (fun file_name -> file_name |> load_and_parse)
      >> Linker.link_progs >> Semantic_analysis.type_check
      >> Codegen.to_llvm_module
      >> (fun llmodule ->
           if verify_module then Llvm_analysis.assert_valid_module llmodule;
           Llvm_analysis.assert_valid_module llmodule;
           llmodule)
      >> if optimize then Optimizer.optimize_module else Fun.id)
      >> fun llmodule ->
      assert (Llvm_bitwriter.write_bitcode_file llmodule outputfile)

let handle_syntatic_error source lexeme_pos msg =
  let lines = String.split_on_char '\n' source in
  let line = List.nth lines (lexeme_pos.Location.line - 1) in
  let prefix = String.make (lexeme_pos.Location.start_column - 1) ' ' in
  let middle =
    String.make
      (lexeme_pos.Location.end_column - lexeme_pos.Location.start_column + 1)
      '^'
  in
  Printf.eprintf "\n*** Error at line %d.\n%s\n%s%s\n*** %s\n\n"
    lexeme_pos.Location.line line prefix middle msg

let handle_semantic_error source code_pos msg =
  let lines =
    String.split_on_char '\n' source
    |> List.filteri (fun line _ ->
           code_pos.Location.start_line - 1 <= line
           && line <= code_pos.Location.end_line - 1)
  in
  let length = List.length lines in
  if length = 1 then
    let line = List.hd lines in
    let prefix = String.make (code_pos.Location.start_column - 1) ' ' in
    let middle =
      String.make
        (code_pos.Location.end_column - code_pos.Location.start_column + 1)
        '^'
    in
    Printf.eprintf "\n*** Error at line %d.\n%s\n%s%s\n*** %s\n\n"
      code_pos.Location.start_line line prefix middle msg
  else
    let text = lines |> List.filteri (fun i _ -> i < 5) |> String.concat "\n" in
    Printf.eprintf "\n*** Error at lines %d-%d.\n%s\n*** %s\n\n"
      code_pos.Location.start_line
      (code_pos.Location.start_line + 5)
      text msg

let () =
  let get_source filename = try load_file filename with _ -> "" in
  try
    let action = ref Compile in
    let files = ref [] in
    let outputfile = ref "a.bc" in
    let optimize = ref false in
    let verify = ref false in
    let spec_list =
      [
        ("-p", Arg.Unit (fun () -> action := Parse), "Parse and print AST");
        ( "-t",
          Arg.Unit (fun () -> action := Type_check),
          "Type checks and print the result" );
        ( "-d",
          Arg.Unit (fun () -> action := Dump_llvm_ir),
          "Compile and print the generated LLVM IR" );
        ( "-c",
          Arg.Unit (fun () -> action := Compile),
          "Compile the source file (default)" );
        ( "-o",
          Arg.Set_string outputfile,
          "Place the output into file (default: a.bc)" );
        ( "-O",
          Arg.Set optimize,
          "Optimize the generated LLVM IR (default: false)" );
        ( "-verify",
          Arg.Set verify,
          "Verify the generated LLVM module (default: false)" );
      ]
    in
    let usage =
      Printf.sprintf "Usage:\t%s [options] <source_files>\n" Sys.argv.(0)
    in
    Arg.parse spec_list (fun file -> files := file :: !files) usage;
    if List.length !files = 0 then Arg.usage spec_list usage
    else
      try action_function !outputfile !optimize !verify !action !files with
      | Scanner.Lexing_error (pos, msg) | Parsing.Syntax_error (pos, msg) ->
          handle_syntatic_error (get_source pos.Location.file) pos msg
      | Semantic_analysis.Semantic_error (pos, msg) ->
          handle_semantic_error (get_source pos.Location.file) pos msg
  with Sys_error msg -> Printf.eprintf "*** Error %s ***\n" msg
