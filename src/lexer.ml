type lexer_t =
  { tokens : Token.token_t list }

let symbols : (char, Token.tokentype_t) Hashtbl.t = Hashtbl.create 10

let keywords : (string, Token.tokentype_t) Hashtbl.t = Hashtbl.create 10

let populate_symbols () : unit =
  let _ = Hashtbl.add symbols '(' Token.LParen in
  let _ = Hashtbl.add symbols ')' Token.RParen in
  let _ = Hashtbl.add symbols '=' Token.Equals in
  let _ = Hashtbl.add symbols ':' Token.Colon in
  let _ = Hashtbl.add symbols ';' Token.SemiColon in
  let _ = Hashtbl.add symbols ',' Token.Comma in
  let _ = Hashtbl.add symbols '+' Token.Plus in
  let _ = Hashtbl.add symbols '*' Token.Mult in
  let _ = Hashtbl.add symbols '/' Token.Binop in (* TODO: make own type. *)
  let _ = Hashtbl.add symbols '<' Token.LessThan in
  let _ = Hashtbl.add symbols '>' Token.GreaterThan in
  ()

let populate_keywords () : unit =
  let _ = Hashtbl.add keywords "let" Token.Let in
  let _ = Hashtbl.add keywords "if" Token.If in
  let _ = Hashtbl.add keywords "then" Token.Then in
  let _ = Hashtbl.add keywords "str" Token.Str in
  let _ = Hashtbl.add keywords "proc" Token.Proc in
  let _ = Hashtbl.add keywords "ret" Token.Ret in
  let _ = Hashtbl.add keywords "exit" Token.Exit in
  let _ = Hashtbl.add keywords "println" Token.Println in
  let _ = Hashtbl.add keywords "i32" Token.I32 in
  let _ = Hashtbl.add keywords "int" Token.Int in
  let _ = Hashtbl.add keywords "u32" Token.U32 in
  let _ = Hashtbl.add keywords "char" Token.Char in
  let _ = Hashtbl.add keywords "struct" Token.Struct in
  let _ = Hashtbl.add keywords "end" Token.End in
  ()

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
  try Some (Hashtbl.find symbols c)
  with Not_found -> None

let peek (lst : char list) (ahead : int) : char option =
  let rec peek' (lst : char list) (i : int) : char option =
    match lst with
    | [] -> None
    | hd :: _ when i = ahead -> Some hd
    | _ :: tl -> peek' tl (i + 1) in
  peek' lst 0

let parse_code (src : string) : lexer_t =
  let rec parse_code' (lst : char list) (acc : Token.token_t list) : lexer_t =
    match lst with
    | [] -> lexer_create (acc @ [Token.token_create_wstr "EOF" Token.EOF])
    | hd :: tl when isignorable hd -> parse_code' tl acc
    | hd :: tl ->
       (match is_identifier hd with
        | Some id -> parse_code' tl (acc @ [Token.token_create_wchar hd id])
        | None ->
           (if isalpha hd || hd = '_' then (* Multichar token. *)
              let multichar, rest = consume_while (hd :: tl) (fun k -> isalnum k || k = '_') in
              match is_keyword multichar with
              | None -> parse_code' rest (acc @ [Token.token_create_wstr multichar Token.ID])
              | Some tokentype -> parse_code' rest (acc @ [Token.token_create_wstr multichar tokentype])

            else if isnum hd then (* Number token. *)
              let intlit, rest = consume_while (hd :: tl) isnum in
              parse_code' rest (acc @ [Token.token_create_wstr intlit Token.IntegerLiteral])

            else if hd = '\'' then (* Character literal token *)
              let charlit, rest = consume_while tl (fun c -> c <> '\'') in
              (* (List.tl rest) to consume extra quote *)
              parse_code' (List.tl rest) (acc @ [Token.token_create_wstr charlit Token.CharLiteral])

            else if hd = '"' then (* String literal token. *)
              let str, rest = consume_while tl (fun c -> c <> '"') in
              (* (List.tl rest) to consume extra quote *)
              parse_code' (List.tl rest) (acc @ [Token.token_create_wstr str Token.StringLiteral])

            else if hd = '-' then (* Minus or right arrow `->` token. *)
              match peek tl 0 with
              | Some k when k = '>' -> parse_code' (List.tl tl) (acc @ [Token.token_create_wstr "->" Token.RightArrow])
              | Some k -> parse_code' tl (acc @ [Token.token_create_wchar hd Token.Binop]) (* TODO: make own type. *)
              | None -> failwith "Lexer ERR: found `None` when parsing `-`"

            else
              failwith (Printf.sprintf "Lexer ERR: unsupported char %c (CODE: %d)\n" hd (int_of_char hd)) ))
  in
  let _ = populate_keywords () in
  let _ = populate_symbols () in
  parse_code' (src |> String.to_seq |> List.of_seq) []

let lexer_dump (lexer : lexer_t) : unit =
  List.iter (fun token -> Token.token_print token) lexer.tokens
