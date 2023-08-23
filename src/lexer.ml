type lexer_t =
  { tokens : Token.token_t list }

let lexer_create (tokens : Token.token_t list) : lexer_t =
  { tokens = tokens }

let identifiers : (char, Token.tokentype_t) Hashtbl.t = Hashtbl.create 10

let populate_identifiers () =
  let _ = Hashtbl.add identifiers '(' Token.LParen in
  let _ = Hashtbl.add identifiers ')' Token.RParen in
  let _ = Hashtbl.add identifiers '=' Token.Equals in
  let _ = Hashtbl.add identifiers ':' Token.Colon in
  let _ = Hashtbl.add identifiers ';' Token.SemiColon in
  let _ = Hashtbl.add identifiers '+' Token.Binop in
  let _ = Hashtbl.add identifiers '-' Token.Binop in
  let _ = Hashtbl.add identifiers '/' Token.Binop in
  let _ = Hashtbl.add identifiers '*' Token.Binop in
  ()

let keywords : (string, Token.tokentype_t) Hashtbl.t = Hashtbl.create 10

let populate_keywords () =
  let _ = Hashtbl.add keywords "let" Token.Let in
  let _ = Hashtbl.add keywords "str" Token.Str in
  let _ = Hashtbl.add keywords "proc" Token.Proc in
  let _ = Hashtbl.add keywords "ret" Token.Ret in
  let _ = Hashtbl.add keywords "i32" Token.I32 in
  let _ = Hashtbl.add keywords "u32" Token.U32 in
  ()

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
   Will NOT consume the character that fails `cond`.
   This is expected to be done by caller. *)
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

let is_identifier (c : char) : Token.tokentype_t option =
  try Some (Hashtbl.find identifiers c)
  with Not_found -> None

let parse_code (src : string) : lexer_t =
  let rec parse_code' (lst : char list) (acc : Token.token_t list) : lexer_t =
    match lst with
    | [] -> lexer_create (acc @ [Token.token_create_wstr "EOF" Token.EOF])
    | hd :: tl when isignorable hd -> parse_code' tl acc
    | hd :: tl ->
       (match is_identifier hd with
        | Some id -> parse_code' tl (acc @ [Token.token_create_wchar hd id])
        | None ->
           (* Multichar token
              - isalpha hd -> is a variable/function name
              - isnum hd -> is an integer literal
              - hd = '"' -> is a string literal *)
           (if isalpha hd then
              let multichar, rest = consume_while (hd :: tl) isalnum in
              match is_keyword multichar with
              | None -> parse_code' rest (acc @ [Token.token_create_wstr multichar Token.ID])
              | Some tokentype -> parse_code' rest (acc @ [Token.token_create_wstr multichar tokentype])

            else if isnum hd then
              let intlit, rest = consume_while (hd :: tl) isnum in
              parse_code' rest (acc @ [Token.token_create_wstr intlit Token.IntegerLiteral])

            else if hd = '"' then
              let str, rest = consume_while tl (fun c -> c <> '"') in
              (* (List.tl rest) to consume extra quote *)
              parse_code' (List.tl rest) (acc @ [Token.token_create_wstr str Token.StringLiteral])

            else
              failwith (Printf.sprintf "unrecognized token %c (CODE: %d)\n" hd (int_of_char hd)) ))
  in
  parse_code' (src |> String.to_seq |> List.of_seq) []

let lexer_dump (lexer : lexer_t) : unit =
  List.iter (fun token -> Token.token_print token) lexer.tokens


let () =
  let filepath = "./input.txt" in

  let read_whole_file filename =
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;

    s in

  let _ = populate_keywords () in
  let _ = populate_identifiers () in
  let src = read_whole_file filepath in
  let lexer = parse_code src in
  lexer_dump lexer
