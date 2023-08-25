let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let filepath = "./input.txt"

let () =
  let src = read_whole_file filepath in
  let lexer = Lexer.parse_code src in
  Lexer.lexer_dump lexer

