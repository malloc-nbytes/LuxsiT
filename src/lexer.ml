type lexer_t =
  { tokens : Token.token_t list
  }

let symbols : (char, Token.tokentype_t) Hashtbl.t = Hashtbl.create 15

let keywords : (string, Token.tokentype_t) Hashtbl.t = Hashtbl.create 20


let populate_symbols () : unit =
  let _ = Hashtbl.add symbols '(' Token.LParen in
  let _ = Hashtbl.add symbols ')' Token.RParen in
  let _ = Hashtbl.add symbols ';' Token.SemiColon in
  let _ = Hashtbl.add symbols ',' Token.Comma in
  let _ = Hashtbl.add symbols '+' Token.Plus in
  let _ = Hashtbl.add symbols '*' Token.Asterisk in
  (* let _ = Hashtbl.add symbols '-' Token.Hyphen in *)
  let _ = Hashtbl.add symbols '/' Token.ForwardSlash in
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
  let _ = Hashtbl.add keywords "const" Token.Const in
  ()


let lexer_create tokens : lexer_t =
  { tokens = tokens }


let isalpha c : bool =
  let c = int_of_char c in
  (c >= 65 && c <= 90) || (c >= 97 && c <= 122)


let isnum c : bool =
  let c = int_of_char c in
  let c = c - int_of_char '0' in
  (c >= 0) && (c <= 9)


let isalnum c : bool =
  isalpha c || isnum c


let isignorable c : bool =
  c = ' ' || c = '\n' || c = '\t'


(* Will consume characters given the predicate `cond`.
   Will NOT consume the character that fails `cond`.
   This is expected to be done by caller. *)
let consume_while lst (cond : 'a -> 'b) : string * char list =
  let rec aux lst acc : string * char list =
    match lst with
    | [] -> acc, []
    | hd :: tl when cond hd -> aux tl (acc ^ String.make 1 hd)
    | hd :: tl -> acc, hd :: tl in
  aux lst ""


(* Checks if a string is one of the reserved keywords. *)
let is_keyword str : Token.tokentype_t option =
  try Some (Hashtbl.find keywords str)
  with Not_found -> None


(* Checks if a string is one of the reserved symbols. *)
let is_symbol c : Token.tokentype_t option =
  try Some (Hashtbl.find symbols c)
  with Not_found -> None


(* Peek `ahead` amount of spaces in the characters. *)
let peek lst ahead : char option =
  let rec peek' lst i : char option =
    match lst with
    | [] -> None
    | hd :: _ when i = ahead -> Some hd
    | _ :: tl -> peek' tl (i + 1) in
  peek' lst 0


(* Create tokens from a string. *)
let lex_tokens src : lexer_t =
  let rec lex_tokens' lst acc : lexer_t =
    match lst with
    | [] -> lexer_create (acc @ [Token.token_create_wstr "EOF" Token.EOF])
    | hd :: tl when isignorable hd -> lex_tokens' tl acc
    | hd :: tl ->
       (match is_symbol hd with
        | Some id -> lex_tokens' tl (acc @ [Token.token_create_wchar hd id])
        | None ->
           (if isalpha hd || hd = '_' then (* Multichar token. *)
              let multichar, rest = consume_while (hd :: tl) (fun k -> isalnum k || k = '_') in
              match is_keyword multichar with
              | None -> lex_tokens' rest (acc @ [Token.token_create_wstr multichar Token.ID])
              | Some tokentype -> lex_tokens' rest (acc @ [Token.token_create_wstr multichar tokentype])

            else if isnum hd then (* Number token. *)
              let intlit, rest = consume_while (hd :: tl) isnum in
              lex_tokens' rest (acc @ [Token.token_create_wstr intlit Token.IntegerLiteral])

            else if hd = '=' then
              match peek tl 0 with
              | Some k when k = '=' -> lex_tokens' (List.tl tl) (acc @ [Token.token_create_wstr "==" Token.Equality])
              | Some k -> lex_tokens' tl (acc @ [Token.token_create_wchar hd Token.Assignment])
              | None -> failwith "Lexer ERR: found `None` when parsing `=`"

            else if hd = '!' then
              match peek tl 0 with
              | Some k when k = '=' -> lex_tokens' (List.tl tl) (acc @ [Token.token_create_wstr "!=" Token.Inequality])
              | Some k -> lex_tokens' tl (acc @ [Token.token_create_wchar hd Token.Bang])
              | None -> failwith "Lexer ERR: found `None` when parsing `!`"

            else if hd = '\'' then (* Character literal token *)
              let charlit, rest = consume_while tl (fun c -> c <> '\'') in
              (* (List.tl rest) to consume extra quote *)
              lex_tokens' (List.tl rest) (acc @ [Token.token_create_wstr charlit Token.CharLiteral])

            else if hd = '"' then (* String literal token. *)
              let str, rest = consume_while tl (fun c -> c <> '"') in
              (* (List.tl rest) to consume extra quote *)
              lex_tokens' (List.tl rest) (acc @ [Token.token_create_wstr str Token.StringLiteral])

            else if hd = ':' then (* Colon or DoubleColon `::` token. *)
              match peek tl 0 with
              | Some k when k = ':' -> lex_tokens' (List.tl tl) (acc @ [Token.token_create_wstr "::" Token.DoubleColon])
              | Some k -> lex_tokens' tl (acc @ [Token.token_create_wchar hd Token.Colon])
              | None -> failwith "Lexer ERR: found `None` when parsing `:`"

            else if hd = '-' then (* Minus or right arrow `->` token. *)
              match peek tl 0 with
              | Some k when k = '>' -> lex_tokens' (List.tl tl) (acc @ [Token.token_create_wstr "->" Token.RightArrow])
              | Some k -> lex_tokens' tl (acc @ [Token.token_create_wchar hd Token.Hyphen])
              | None -> failwith "Lexer ERR: found `None` when parsing `-`"

            else
              failwith (Printf.sprintf "Lexer ERR: unsupported char %c (CODE: %d)\n" hd (int_of_char hd)) ))
  in
  let _ = populate_keywords () in
  let _ = populate_symbols () in
  lex_tokens' (src |> String.to_seq |> List.of_seq) []


(* Print everything in the lexer. Used for debugging. *)
let lexer_dump lexer : unit =
  List.iter (fun token ->
      Printf.printf "[Lexer] %s\n"
        (Token.tokentype_tostr token.Token.tokentype)
    ) lexer.tokens
