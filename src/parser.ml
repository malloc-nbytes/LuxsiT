type node_expr_intlit =
  { intlit : Token.token_t }

type node_expr_id =
  { id : Token.token_t }

type node_expr_t =
  | NodeExprIntlit of node_expr_intlit
  | NodeExprId of node_expr_id

type node_stmt_let =
  { id : Token.token_t;
    expr : node_expr_t }

type node_stmt_exit =
  { expr : node_expr_t }

type node_stmt_t =
  | NodeStmtExit
  | NodeStmtLet

type node_prog_t =
  { stmts : node_stmt_t list }

type parser_t =
  { tokens: Token.token_t list }

let err (msg : string) : unit =
  Printf.printf "(ERR): %s\n" msg

let parser_create (tokens : Token.token_t list) : parser_t =
  { tokens = tokens }

(* Will return the token at `hd`. Does not eat it.*)
let peek (p : parser_t) : Token.token_t option =
  match p.tokens with
  | [] -> None
  | hd :: _ -> Some hd

(* Will return the token at `hd`. Will eat it. *)
let eat (p : parser_t) : parser_t * (Token.token_t option) =
  match p.tokens with
  | [] -> p, None
  | hd :: tl -> parser_create tl, Some hd

(* Will eat the token at `hd`. If `hd.tokentype` does not match
   what is expected, will throw an error. *)
let expect (p : parser_t) (expected_type : Token.tokentype_t) : parser_t * Token.token_t =
  let hd = List.hd p.tokens and tl = List.tl p.tokens in
  if hd.tokentype <> expected_type then
    let _ = err ("unexpected token \"" ^ hd.data ^ "\" expected: " ^ (Token.get_tokentype_as_str expected_type)) in
    failwith "parser error"
  else
    parser_create tl, hd

let parse_expr (p : parser_t) : parser_t * (node_expr_t option) =
  let p, token = eat p in
  match token with
  | Some t when t.tokentype = Token.IntegerLiteral -> p, Some (NodeExprIntlit { intlit = t })
  | Some t when t.tokentype = Token.ID -> p, Some (NodeExprId { id = t })
  | None -> p, None
  | _ ->
     let _ = err "cannot create expression" in
     failwith "parser error"

let rec parse_exit (p : parser_t) : node_exit_t option =
  let rec parse_exit' (p : parser_t) (node_exit : node_exit_t option) : node_exit_t option =
    match peek p with
    | None -> node_exit
    | Some t when t.tokentype = Token.EOF -> node_exit
    | Some t when t.tokentype = Token.Exit ->
       let p, _ = eat p in    (* eat `exit` *)
       let p, _ = expect p Token.LParen in
       let p, node_expr = parse_expr p in
       let p, _ = expect p Token.RParen in
       let p, _ = expect p Token.SemiColon in
       (match node_expr with
        | Some expr -> parse_exit' p (Some (node_exit_create expr))
        | None -> failwith "(ERR) invalid expression")
    | Some t ->
       let _ = err ("unimplemented token: " ^ (Token.get_tokentype_as_str t.tokentype) ^ "\n") in
       failwith "parser error"
  in
  parse_exit' p None

