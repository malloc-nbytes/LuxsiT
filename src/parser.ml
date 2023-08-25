type node_expr_t =
  { intlit : Token.token_t }

type node_ret_t =
  { expr : node_expr_t }

let node_expr_create (intlit : Token.token_t) : node_expr_t =
  { intlit = intlit }

let node_ret_create (expr : node_expr_t) : node_ret_t =
  { expr = expr }

type parser_t =
  { tokens: Token.token_t list }

let parser_create (tokens : Token.token_t list) : parser_t =
  { tokens = tokens }

let peek (p : parser_t) : Token.token_t option =
  match p.tokens with
  | [] -> None
  | hd :: _ -> Some hd

let eat (p : parser_t) : parser_t * (Token.token_t option) =
  match p.tokens with
  | [] -> p, None
  | hd :: tl -> parser_create tl, Some hd

let expect (p : parser_t) (expected_type : Token.tokentype_t) : parser_t * Token.token_t =
  let hd = List.hd p.tokens and tl = List.tl p.tokens in
  if hd.tokentype <> expected_type then
    let _ = Printf.printf "unexpected token \"%s\" expected: %s\n" hd.data (Token.get_tokentype_as_str hd.tokentype) in
    failwith "parse ERR"
  else
    parser_create tl, hd

let parse_expr (p : parser_t) : parser_t * (node_expr_t option) =
  let p, token = eat p in
  match token with
  | Some t -> p, Some (node_expr_create t)
  | None -> p, None

let rec parse (p : parser_t) : node_ret_t option =
  let rec parse' (p : parser_t) (node_ret : node_ret_t option) : node_ret_t option =
    match peek p with
    | None -> node_ret
    | Some t when t.tokentype = Token.EOF -> failwith "EOF"
    | Some t when t.tokentype = Token.Ret ->
       let p, _ = eat p in    (* eat `ret` *)
       let p, node_expr = parse_expr p in
       let p, _ = expect p Token.SemiColon in (* eat `;` *)
       (match node_expr with
        | Some expr -> parse' p (Some (node_ret_create expr))
        | None -> failwith "invalid expression")
    | Some t ->
       let _ = Printf.printf "unimplemented token: %s\n" (Token.get_tokentype_as_str t.tokentype) in
       failwith "unimplemented token error"
  in
  parse' p None

