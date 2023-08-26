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
  | NodeStmtExit of node_expr_t
  | NodeStmtLet of node_stmt_let

type node_prog_t =
  { stmts : node_stmt_t list }

type parser_t =
  { tokens: Token.token_t list }

let err (msg : string) : unit =
  Printf.printf "(ERR) %s\n" msg

let parser_create (tokens : Token.token_t list) : parser_t =
  { tokens = tokens }

(* Will return the token at `hd`. Does not eat it.*)
let peek (p : parser_t) : Token.token_t option =
  match p.tokens with
  | [] -> None
  | hd :: _ ->
     let type_ = Token.get_tokentype_as_str hd.tokentype in
     let _ = Printf.printf "peeking: %s\n" type_ in
     Some hd

(* Will return the token at `hd`. Will eat it. *)
let eat (p : parser_t) : parser_t * (Token.token_t option) =
  match p.tokens with
  | [] -> p, None
  | hd :: tl ->
     let type_ = Token.get_tokentype_as_str hd.tokentype in
     let _ = Printf.printf "eating: %s\n" type_ in
     parser_create tl, Some hd

(* Will eat the token at `hd`. If `hd.tokentype` does not match
   what is expected, will throw an error. *)
let expect (p : parser_t) (expected_type : Token.tokentype_t) : parser_t * Token.token_t =
  let hd = List.hd p.tokens and tl = List.tl p.tokens in
  if hd.tokentype <> expected_type then
    let _ = err ("unexpected token \"" ^ hd.data ^ "\" expected: " ^ (Token.get_tokentype_as_str expected_type)) in
    failwith "parser error"
  else
    let type_ = Token.get_tokentype_as_str hd.tokentype in
    let _ = Printf.printf "expecting: %s\n" type_ in
    parser_create tl, hd

let parse_expr (p : parser_t) : parser_t * (node_expr_t option) =
  let p, token = eat p in
  match token with
  | Some t when t.tokentype = Token.IntegerLiteral -> p, Some (NodeExprIntlit { intlit = t })
  | Some t when t.tokentype = Token.ID -> p, Some (NodeExprId { id = t })
  | None -> p, None
  | _ ->
     let _ = err "cannot parse expression" in
     failwith "parser error"

let parse_stmt (p : parser_t) : parser_t * (node_stmt_t option) =
  print_endline "parse_stmt";
  match peek p with
  | Some t when t.tokentype = Token.Exit ->
     print_endline "token.exit";
     let p, _ = eat p in        (* eat exit *)
     let p, _ = expect p Token.LParen in
     let p, node_expr = parse_expr p in
     (match node_expr with
      | Some expr ->
         let p, _ = expect p Token.RParen in
         let p, _ = expect p Token.SemiColon in
         p, Some (NodeStmtExit expr)
      | None ->
         let _ = err "expected an expression after 'exit('" in
         failwith "parser error")
  | Some t when t.tokentype = Token.Let ->
     print_endline "token.Let";
     let p, _ = eat p in        (* eat let *)
     let p, id = expect p Token.ID in
     let p, _ = expect p Token.Equals in
     let p, expr = parse_expr p in
     let p, _ = expect p Token.SemiColon in
     (match expr with
      | Some e -> p, Some (NodeStmtLet { id = id; expr = e })
      | None ->
         let _ = err "invalid expression after 'let `ID` ='" in
         failwith "parser error")
  | _ -> p, None (* no stmt found *)

let parse_program (p : parser_t) : node_prog_t =
  let rec parse_program' (p : parser_t) (program : node_prog_t) : parser_t * node_prog_t =
    let p', stmt = parse_stmt p in
    match stmt with
    | Some stmt -> parse_program' p' { stmts = program.stmts @ [stmt] }
    | None -> p', program
  in
  let final_parser_state, result = parse_program' p { stmts = [] } in
  result
