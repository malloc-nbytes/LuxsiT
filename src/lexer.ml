let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let keywords : (string, Token.tokentype_t) Hashtbl.t = Hashtbl.create 10

let populate_keywords () =
  let _ = Hashtbl.add keywords "let" Token.Let in
  let _ = Hashtbl.add keywords "str" Token.Str in
  let _ = Hashtbl.add keywords "proc" Token.Proc in
  let _ = Hashtbl.add keywords "ret" Token.Ret in
  let _ = Hashtbl.add keywords "i32" Token.I32 in
  ()

type lexer_t =
  { tokens : Token.token_t list }

let lexer_create (tokens : Token.token_t list) : lexer_t =
  { tokens = tokens }

let isalpha (c : char) : bool =
  let c = int_of_char c in
  (c >= 65 && c <= 90) || (c >= 97 && c <= 122)

let isnum (c : char) : bool =
  let c = int_of_char c in
  let c = c - int_of_char '0' in
  (c >= 0) && (c <= 9)

let isalnum (c : char) : bool = isalpha c || isnum c

let isignorable (c : char) : bool = c = ' ' || c = '\n' || c = '\t'

(* Will consume characters given the predicate `cond`.
   If this function is being passed a pattern matched list,
   `hd` & `tl`, then `hd` should be @ `tl`. Will NOT consume
   the character that fails `cond`. This is expected to be 
   done by caller. *)
let consume_while (lst : char list) (cond : char -> bool) : string * char list =
  let rec aux (lst : char list) (acc : string) : string * char list =
    match lst with
    | [] -> acc, []
    | hd :: tl when cond hd -> aux tl (acc ^ String.make 1 hd)
    | hd :: tl -> acc, hd :: tl in
  aux lst ""

let is_keyword (str : string) : Token.tokentype_t option =
  try Some (Hashtbl.find keywords str)
  with Not_found -> None

let parse_code (src : string) : lexer_t =
  let rec parse_code' (lst : char list) (acc : Token.token_t list) : lexer_t =
    match lst with
    | [] -> lexer_create (acc @ [Token.token_create_wstr "EOF" Token.EOF])
    | hd :: tl when hd = '(' ->
       parse_code' tl (acc @ [Token.token_create_wchar hd Token.RParen])
    | hd :: tl when hd = ')' ->
       parse_code' tl (acc @ [Token.token_create_wchar hd Token.LParen])
    | hd :: tl when hd = '=' ->
       parse_code' tl (acc @ [Token.token_create_wchar hd Token.Equals])
    | hd :: tl when hd = ':' ->
       parse_code' tl (acc @ [Token.token_create_wchar hd Token.Colon])
    | hd :: tl when hd = ';' ->
       parse_code' tl (acc @ [Token.token_create_wchar hd Token.SemiColon])
    | hd :: tl when hd = '+' || hd = '-' || hd = '*' || hd = '/' ->
       parse_code' tl (acc @ [Token.token_create_wchar hd Token.Binop])
    | hd :: tl when isignorable hd -> parse_code' tl acc
    | hd :: tl ->
       (* Multichar token *)
       (* NOTE: consume_while does not consume failing char! *)
       if isalpha hd then       (* Is a variable/function name. *)
         let multichar, rest = consume_while (hd :: tl) isalnum in
         match is_keyword multichar with
         | None -> parse_code' rest (acc @ [Token.token_create_wstr multichar Token.ID])
         | Some tokentype -> parse_code' rest (acc @ [Token.token_create_wstr multichar tokentype])

       else if isnum hd then    (* Is an integer literal. *)
         let intlit, rest = consume_while (hd :: tl) isnum in
         parse_code' rest (acc @ [Token.token_create_wstr intlit Token.IntegerLiteral])

       else if hd = '"' then    (* Is a string literal. *)
         let str, rest = consume_while tl (fun c -> c <> '"') in
         (* (List.tl rest) to consume extra '"' *)
         parse_code' (List.tl rest) (acc @ [Token.token_create_wstr str Token.StringLiteral])

       else                     (* Something else. *)
         failwith (Printf.sprintf "unrecognized token %c (CODE: %d)\n" hd (int_of_char hd))
  in
  parse_code' (src |> String.to_seq |> List.of_seq) []

let lexer_dump (lexer : lexer_t) : unit =
  List.iter (fun token ->
      match token.Token.tokentype with
      | Token.Let -> Printf.printf "Let: %s\n" token.Token.data
      | Token.Str -> Printf.printf "Str: %s\n" token.Token.data
      | Token.I32 -> Printf.printf "I32: %s\n" token.Token.data
      | Token.Colon -> Printf.printf "Colon: %s\n" token.Token.data
      | Token.SemiColon -> Printf.printf "SemiColon: %s\n" token.Token.data
      | Token.Proc -> Printf.printf "Proc: %s\n" token.Token.data
      | Token.Ret -> Printf.printf "Ret: %s\n" token.Token.data
      | Token.LParen -> Printf.printf "LParen: %s\n" token.Token.data
      | Token.RParen -> Printf.printf "RParen: %s\n" token.Token.data
      | Token.ID -> Printf.printf "ID: %s\n" token.Token.data
      | Token.IntegerLiteral -> Printf.printf "IntegerLiteral: %s\n" token.Token.data
      | Token.StringLiteral -> Printf.printf "StringLiteral: %s\n" token.Token.data
      | Token.Equals -> Printf.printf "Equals: %s\n" token.Token.data
      | Token.Binop -> Printf.printf "Binop: %s\n" token.Token.data
      | Token.EOF -> Printf.printf "EOF: %s\n" token.Token.data
    ) lexer.tokens

let filepath = "./input.txt"
let () =
  let _ = populate_keywords () in
  let src = read_whole_file filepath in
  let lexer = parse_code src in
  lexer_dump lexer
