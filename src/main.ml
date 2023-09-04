type command =
  | Assemble
  | Link

let input_filepath = "./input.lux"
let output_filepath = "./out.asm"
let assemble_cmd = "nasm -felf64 " ^ output_filepath
let link_cmd = "ld -o ./out ./out.o"

let read_input_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let write_to_file (filepath : string) (asm : string) : unit =
  let oc = open_out filepath in
  let _ = output_string oc asm in
  close_out oc
 
let perform_cmd (filepath : string) (_cmd : command) : unit =
  let _cmd, action = match _cmd with
    | Assemble -> assemble_cmd, "assembly"
    | Link -> link_cmd, "linking" in
  let exit_code = Sys.command _cmd in
  (Printf.printf "(INFO) %s exited with code %d\n" action exit_code)

(* TODO: add cli args and usage. *)
(* TODO: take in a file. *)
let () =
  let src = read_input_file input_filepath in
  let lexer = Lexer.parse_code src in
  (* Lexer.lexer_dump lexer; *)
  let parse = Parser.parser_create lexer.tokens in
  let prog : Parser.node_prog_t = Parser.parse_program parse.tokens in

  let output = Gen.generate_program prog in

  let _ = write_to_file output_filepath output in
  let _ = perform_cmd output_filepath Assemble in
  perform_cmd output_filepath Link
