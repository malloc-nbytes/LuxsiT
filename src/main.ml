type cmd =
  | Assemble
  | Link

let assemble_cmd = "nasm -felf64 out.asm"
let link_cmd = "ld -o out out.o"
let input_filepath = "../input.txt"
let output_filepath = "./out.asm"

let read_input_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let write_to_file (filepath : string) (asm : string) : unit =
  let oc = open_out filepath in
  let _ = output_string oc asm in
  close_out oc

let perform_cmd (filepath : string) (_cmd : cmd) : unit =
  let _cmd, action = match _cmd with
    | Assemble -> assemble_cmd, "assembly"
    | Link -> link_cmd, "linking" in

  let exit_code = Sys.command _cmd in
  (Printf.printf "(INFO) %s exited with code %d\n" action exit_code)

let () =
  let src = read_input_file input_filepath in
  let lexer = Lexer.parse_code src in
  let parse = Parser.parser_create lexer.tokens in
  let tree : Parser.node_prog_t = Parser.parse_program parse in
  ()
