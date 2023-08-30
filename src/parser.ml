type node_term_intlit_t =
  { intlit : Token.token_t }

type node_term_id_t =
  { id : Token.token_t }

and node_bin_expr_mult_t =
  { lhs : node_expr_t;
    rhs : node_expr_t }

and node_bin_expr_add_t =
  { lhs : node_expr_t;
    rhs : node_expr_t }

and node_bin_expr_t =
  | NodeBinExprAdd of node_bin_expr_add_t
  | NodeBinExprMult of node_bin_expr_mult_t

and node_term_t =
  | NodeTermIntlit of node_term_intlit_t
  | NodeTermId of node_term_id_t

and node_expr_t =
  | NodeTerm of node_term_t
  | NodeBinaryExpr of node_bin_expr_t

type node_stmt_let_t =
  { id : Token.token_t;
    expr : node_expr_t }

type node_stmt_exit =
  { expr : node_expr_t }

type node_stmt_t =
  | NodeStmtExit of node_expr_t
  | NodeStmtLet of node_stmt_let_t

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
  else parser_create tl, hd

let parse_term (p : parser_t) : parser_t * (node_term_t option) =
  match peek p with
  | Some t when t.tokentype = Token.IntegerLiteral -> p, Some (NodeTermIntlit { intlit = t })
  | Some t when t.tokentype = Token.ID -> p, Some (NodeExprId { id = t })
  | None -> p, None

(* let rec parse_bin_expr (p : parser_t) : parser_t * (node_bin_expr_t option) = *)
(*   let p, lhs = parse_expr p in *)
(*   match lhs with *)
(*   | None -> p, None *)
(*   | Some lhs_expr -> *)
(*      match peek p with *)
(*      | Some t when t.tokentype == Token.Plus -> *)
(*         let p, _ = expect p Token.Plus in *)
(*         let p, rhs = parse_expr p in *)
(*         (match rhs with *)
(*          | Some rhs_expr -> p, Some (NodeBinExprAdd { lhs = lhs_expr; rhs = rhs_expr }) *)
(*          | None -> *)
(*             let _ = err "Expected expression" in *)
(*             failwith "parser error") *)
(*      | Some t when t.tokentype == Token.Mult -> failwith "todo: multiplication" *)
(*      | _ -> failwith "peek failed" *)

and parse_expr (p : parser_t) : parser_t * (node_expr_t option) =
  let term = parse_term p in
  let p, token = eat p in
  match token with
  | Some t when t.tokentype = Token.IntegerLiteral -> p, Some (NodeExprIntlit { intlit = t })
  | Some t when t.tokentype = Token.ID -> p, Some (NodeExprId { id = t })
  | None -> p, None
  | _ -> failwith "todo"
     (* let p, expr = parse_bin_expr p in *)
     (* (match expr with *)
     (* | Some expr -> p, Some (NodeBinaryExpr expr) *)
     (* | None -> *)
     (*    let _ = err "cannot parse expression" in *)
     (*    failwith "parser error") *)

let parse_stmt (p : parser_t) : parser_t * (node_stmt_t option) =
  match peek p with
  | Some t when t.tokentype = Token.Exit ->
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
  let _, result = parse_program' p { stmts = [] } in
  result
