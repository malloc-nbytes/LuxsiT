let input_filepath = "./input.lux"
let output_filepath = "./out.asm"
let assemble_cmd = "nasm -felf64 " ^ output_filepath
let link_cmd = "ld -o ./out ./out.o"


(* Read input file for src code. *)
let read_input_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s


(* Write the generated assembly out to file. *)
let write_to_file filepath asm : unit =
  let oc = open_out filepath in
  let _ = output_string oc asm in
  close_out oc


let assemble filepath : unit =
  let exit_code = Sys.command assemble_cmd in
  Printf.printf "(INFO) Assembler exited with code %d\n" exit_code


let link filepath : unit =
  let exit_code = Sys.command link_cmd in
  Printf.printf "(INFO) Linker exited with code %d\n" exit_code


let main () =
  let src = read_input_file input_filepath in
  let lexer = Lexer.lex_tokens src in
  Lexer.lexer_dump lexer;

  let parser = Parser.parser_create lexer.tokens in
  let program = Parser.parse_program parser.tokens in

  let output = Gen.generate_program program in

  let _ = write_to_file output_filepath output in
  let _ = assemble output_filepath in
  link output_filepath


let () = main ()
