type node_stmt_t =
  | NodeStmtExit      of node_stmt_exit_t
  | NodeStmtVarDecl   of node_stmt_var_decl_t
  | NodeStmtPrintln   of node_stmt_println_t
  | NodeStmtMutateVar of node_stmt_mutate_var_t
  | NodeStmtIf        of node_if_t

and node_term_t =
  | NodeTermID     of node_term_id_t
  | NodeTermIntLit of node_term_intlit

and node_expr_t =
  | NodeBinExpr of node_bin_expr_t
  | NodeTerm    of node_term_t

and node_bin_expr_t =
  { lhs : node_expr_t
  ; rhs : node_expr_t
  ; op  : string
  }

and node_if_t =
  { expr  : node_expr_t
  ; stmts : node_stmt_t list
  }

and node_prog_t =
  { stmts : node_stmt_t list
  }

and node_term_id_t =
  { id : Token.token_t
  }

and node_stmt_mutate_var_t =
  { id   : Token.token_t
  ; expr : node_expr_t
  }

and node_term_intlit =
  { intlit : Token.token_t
  }

and node_stmt_exit_t =
  { expr : node_expr_t
  }

and node_stmt_var_decl_t =
  { id       : Token.token_t
  ; expr     : node_expr_t option
  ; constant : bool
  }

and node_stmt_println_t =
  { expr : node_expr_t
  }

type parser_t =
  { tokens : Token.token_t list
  }

let parser_create tokens : parser_t =
  { tokens }

let expect (p : parser_t) (expected_type : Token.tokentype_t) : parser_t * Token.token_t =
  match p.tokens with
  | [] -> failwith "no more tokens"
  | hd :: tl when hd.tokentype = expected_type -> { tokens = tl }, hd
  | hd :: _ ->
     let _ = Err.err ("expected token " ^ (Token.tokentype_tostr expected_type) ^ " but got " ^ hd.data) in 
     failwith "expected token"

let eat (p : parser_t) : parser_t * Token.token_t =
  match p.tokens with
  | [] ->
     let _ = Err.err "no tokens error" in
     failwith "parser error"
  | hd :: tl -> { tokens = tl }, hd

let at (p : parser_t) : Token.token_t =
  match p.tokens with
  | [] ->
     let _ = Err.err "no tokens error" in
     failwith "parser error"
  | hd :: _ -> hd

let rec parse_primary_expr (p : parser_t) : parser_t * node_expr_t =
  match at p with
  | t when t.tokentype = Token.ID ->
     let p, t = eat p in
     p, NodeTerm (NodeTermID { id = t })
  | t when t.tokentype = Token.IntegerLiteral ->
     let p, t = eat p in
     p, NodeTerm (NodeTermIntLit { intlit = t })
  | t when t.tokentype = Token.LParen ->
     let p, _ = eat p in
     let p, expr = parse_expr p in
     let p, _ = expect p Token.RParen in
     p, expr
  | _ ->
     let _ = Err.err ("could not parse primary expression. unexpected token " ^ (at p).data) in 
     failwith "unexpected token"

and parse_equality_expr (p : parser_t) : parser_t * node_expr_t =
  let p, lhs = parse_primary_expr p in
  let rec parse_equality_expr (p : parser_t) (lhs : node_expr_t) : parser_t * node_expr_t =
    match at p with
    | t when t.tokentype = Token.Equality || t.tokentype = Token.Inequality || t.tokentype = Token.GreaterThan || t.tokentype = Token.LessThan || t.tokentype = Token.GreaterThanEqual || t.tokentype = Token.LessThanEqual ->
       let p, t = eat p in
       let p, rhs = parse_primary_expr p in
       parse_equality_expr p (NodeBinExpr { lhs = lhs; rhs = rhs; op = t.data })
    | _ -> p, lhs in
  parse_equality_expr p lhs

and parse_mult_expr (p : parser_t) : parser_t * node_expr_t =
  let p, lhs = parse_equality_expr p in
  let rec parse_mult_expr (p : parser_t) (lhs : node_expr_t) : parser_t * node_expr_t =
    match at p with
    | t when t.tokentype = Token.Asterisk || t.tokentype = Token.ForwardSlash ->
       let p, t = eat p in
       let p, rhs = parse_equality_expr p in
       parse_mult_expr p (NodeBinExpr { lhs = lhs; rhs = rhs; op = t.data })
    | _ -> p, lhs in
  parse_mult_expr p lhs

and parse_add_expr (p : parser_t) : parser_t * node_expr_t =
  let p, lhs = parse_mult_expr p in
  let rec parse_add_expr (p : parser_t) (lhs : node_expr_t) : parser_t * node_expr_t =
    match at p with
    | t when t.tokentype = Token.Plus || t.tokentype = Token.Hyphen ->
       let p, t = eat p in
       let p, rhs = parse_mult_expr p in
       parse_add_expr p (NodeBinExpr { lhs = lhs; rhs = rhs; op = t.data })
    | _ -> p, lhs in
  parse_add_expr p lhs

and parse_expr (p : parser_t) : parser_t * node_expr_t =
  let p, expr = parse_add_expr p in
  p, expr

and parse_var_decl (p : parser_t) : parser_t * node_stmt_t =
  let p, t = eat p in
  let constant = t.tokentype = Token.Const in
  let p, id = expect p Token.ID in
  if (at p).tokentype = Token.SemiColon then
    let _ = if constant then Err.err "constant must be initialized" in
    let p, _ = eat p in
    p, NodeStmtVarDecl { id; expr = None; constant }
  else
    let p, _ = expect p Token.Assignment in
    let p, expr = parse_expr p in
    let p, _ = expect p Token.SemiColon in
    p, NodeStmtVarDecl { id; expr = Some expr; constant }

and parse_stmts (p : parser_t) : parser_t * node_stmt_t list =
  match at p with
  | t when t.tokentype = Token.End -> p, []
  | _ ->
     let p, stmt = parse_stmt p in
     let p, stmts = parse_stmts p in
     p, stmt :: stmts

and parse_stmt (p : parser_t) : parser_t * node_stmt_t =
  match at p with
  | t when t.tokentype = Token.Exit ->
     let p, _ = eat p in
     let p, expr = parse_expr p in
     let p, _ = expect p Token.SemiColon in
     p, NodeStmtExit { expr }
  | t when t.tokentype = Token.Let || t.tokentype = Token.Const ->
     parse_var_decl p
  | t when t.tokentype = Token.Println ->
     let p, _ = eat p in
     let p, expr = parse_expr p in
     let p, _ = expect p Token.SemiColon in
     p, NodeStmtPrintln { expr }
  | t when t.tokentype = Token.ID ->
     let p, id = eat p in
     let p, _ = expect p Token.Assignment in
     let p, expr = parse_expr p in
     let p, _ = expect p Token.SemiColon in
     p, NodeStmtMutateVar { id; expr }
  | t when t.tokentype = Token.If ->
     let p, _ = eat p in
     let p, expr = parse_expr p in
     let p, _ = expect p Token.Then in
     let p, stmts = parse_stmts p in
     let p, _ = expect p Token.End in
     p, NodeStmtIf { expr; stmts }
  | _ ->
     let _ = Err.err ("unexpected token " ^ (at p).data) in
     failwith "parser error"

(* Entrypoint. *)
let parse_program (tokens : Token.token_t list) : node_prog_t =
  let p = { tokens } in
  let rec produce_program (p : parser_t) (program : node_prog_t) : node_prog_t =
    match p.tokens with
    | [] -> program
    | hd :: _ when hd.tokentype = Token.EOF -> program
    | _ -> 
       let p, stmt = parse_stmt p in
       produce_program p { stmts = program.stmts @ [stmt] }
  in
  produce_program p { stmts = [] }
