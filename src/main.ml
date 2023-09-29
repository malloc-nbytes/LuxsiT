(* Read input file for src code. *)
let read_input_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(* Write the generated assembly out to file. *)
let write_to_file filepath asm =
  let oc = open_out filepath in
  let _ = output_string oc asm in
  close_out oc

let assemble output_filepath =
  let exit_code = Sys.command ("nasm -felf64 " ^ output_filepath) in
  Printf.printf "[LuxsiT] Assembler exited with code %d\n" exit_code

let link filepath =
  let exit_code = Sys.command "ld -o ./out ./out.o" in
  Printf.printf "[LuxsiT] Linker exited with code %d\n" exit_code

let main () =
  let input_filepath = "./input.lux" in
  let output_filepath = "./out.asm" in

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
