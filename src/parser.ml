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
  | NodeTermParen of unit

and node_expr_t =
  | NodeTerm of node_term_t
  | NodeBinaryExpr of node_bin_expr_t

and node_stmt_let_t =
  { id : Token.token_t;
    expr : node_expr_t }

type node_stmt_t =
  | NodeStmtExit of node_expr_t
  | NodeStmtPrintln of node_expr_t
  | NodeStmtLet of node_stmt_let_t

type node_prog_t =
  { stmts : node_stmt_t list }

type parser_t =
  { tokens: Token.token_t list }

let err (msg : string) : unit =
  Printf.printf "(ERR) %s\n" msg

let parser_create (tokens : Token.token_t list) : parser_t =
  { tokens = tokens }

let unwrap (k : 'a option) : 'a =
  match k with
  | Some x -> x
  | None -> 
     let _ = err "called unwrap on None value" in
     failwith "(fatal) parser error"

let print_tokens (tokens : Token.token_t list) : unit =
  let _ = List.iter (fun t -> Printf.printf "%s" ((Token.get_tokentype_as_str t.Token.tokentype))) tokens in
  print_endline ""

let rec print_expr (expr : node_expr_t) : unit =
  let print_term (term : node_term_t) : unit =
    match term with
    | NodeTermIntlit intlit -> print_endline ("NodeTermIntlit: " ^ intlit.intlit.data)
    | NodeTermId id -> print_endline ("NodeTermId: " ^ id.id.data)
    | NodeTermParen () -> print_endline "NodeTermParen" in
  let print_bin_expr (bin_expr : node_bin_expr_t) : unit =
    match bin_expr with
    | NodeBinExprAdd add -> 
        print_endline "NodeBinExprAdd";
        print_expr add.lhs;
        print_expr add.rhs
    | NodeBinExprMult mult -> 
        print_endline "NodeBinExprMult";
        print_expr mult.lhs;
        print_expr mult.rhs in
  match expr with
  | NodeTerm term -> print_term term
  | NodeBinaryExpr bin_expr -> print_bin_expr bin_expr

(* Will return the token at `hd`. Does not eat it.*)
let peek (p : parser_t) : Token.token_t option =
  match p.tokens with
  | [] -> None
  | hd :: _ -> Some hd

(* Will return the token at `hd`. Will eat it. *)
let eat (p : parser_t) : parser_t * (Token.token_t option) =
  let _ = print_endline ("eat: " ^ (Token.get_tokentype_as_str (unwrap (peek p)).tokentype)) in
  let _ = print_tokens p.tokens in
  match p.tokens with
  | [] -> p, None
  | hd :: tl -> parser_create tl, Some hd

(* Will eat the token at `hd`. If `hd.tokentype` does not match
   what is expected, will throw an error. *)
let expect (p : parser_t) (expected_type : Token.tokentype_t) : parser_t * Token.token_t =
  let _ = print_endline ("expect: " ^ (Token.get_tokentype_as_str expected_type)) in
  let _ = print_tokens p.tokens in
  let hd = List.hd p.tokens and tl = List.tl p.tokens in
  if hd.tokentype <> expected_type then
    let _ = err ("unexpected token \"" ^ hd.data ^ "\" expected: " ^ (Token.get_tokentype_as_str expected_type)) in
    failwith "parser error"
  else parser_create tl, hd

let rec parse_term (p : parser_t) : parser_t * (node_term_t option) =
  let _ = print_endline "parse_term" in
  let _ = print_tokens p.tokens in
  match peek p with
  (* Handle integer literals and identifiers *)
  | Some t when t.tokentype = Token.IntegerLiteral -> p, Some (NodeTermIntlit { intlit = t })
  | Some t when t.tokentype = Token.ID -> p, Some (NodeTermId { id = t })
  | Some t when t.tokentype = Token.LParen -> p, Some (NodeTermParen ())
  | None -> p, None
  | _ ->
    let _ = err ("unexpected token \"" ^ (Token.get_tokentype_as_str (unwrap (peek p)).tokentype) ^ "\" in term") in
    failwith "parser error"

and parse_multiplicative_expr (p : parser_t) : parser_t * (node_expr_t option) =
  let _ = print_endline "parse_multiplicative_expr" in
  let _ = print_tokens p.tokens in
  let p, term = parse_term p in
  if term <> None then
    let p, _ = eat p in (* eat term i.e. 1 * 2, `1` is a term, it gets eaten. *)
    match peek p with
    | Some t when t.tokentype = Token.Mult ->
       let p, _ = expect p Token.Mult in
       let p, rhs = parse_multiplicative_expr p in
       (match rhs with
        | Some rhs_expr -> p, Some (NodeBinaryExpr (NodeBinExprMult { lhs = NodeTerm (unwrap term); rhs = rhs_expr }))
        | None ->
           let _ = err "expected expression after '*'" in
           failwith "parser error")
    | _ -> p, Some (NodeTerm (unwrap term))
  else
    p, None

and parse_additive_expr (p : parser_t) : parser_t * (node_expr_t option) =
  let _ = print_endline "parse_additive_expr" in
  let _ = print_tokens p.tokens in
  let p, term = parse_multiplicative_expr p in
  if term <> None then
    (* let p, _ = eat p in *) (* do not eat here, does so in parse_multiplicative_expr *)
    match peek p with
    | Some t when t.tokentype = Token.Plus ->
       let p, _ = expect p Token.Plus in
       let p, rhs = parse_additive_expr p in
       (match rhs with
        | Some rhs_expr -> p, Some (NodeBinaryExpr (NodeBinExprAdd { lhs = unwrap term; rhs = rhs_expr }))
        | None ->
           let _ = err "expected expression after '+'" in
           failwith "parser error")
    | _ -> p, Some (unwrap term)
  else
    p, None

and parse_expr (p : parser_t) : parser_t * (node_expr_t option) =
  let _ = print_endline "parse_expr" in
  let _ = print_tokens p.tokens in
  let p, term = parse_term p in
  match term with
  | Some NodeTermParen _ ->
      let p, _ = expect p Token.LParen in
      let p, expr = parse_expr p in
      let p, _ = expect p Token.RParen in
      (match expr with
      | Some expr -> p, Some expr
      | None ->
        let _ = err "expected expression after '(' in parenthetical expression" in
        failwith "parser error")
  | Some term ->
      let p, _ = eat p in (* eat term i.e. 1 + 2, `1` is a term, it gets eaten. *)
      (match peek p with (* figure out which binary expression to use *)
      | Some t when t.tokentype = Token.Plus ->
        let p, _ = expect p Token.Plus in
        let p, rhs = parse_additive_expr p in
        (match rhs with
          | Some rhs_expr -> p, Some (NodeBinaryExpr (NodeBinExprAdd { lhs = NodeTerm term; rhs = rhs_expr }))
          | None ->
            let _ = err "expected expression after '+'" in
            failwith "parser error")
      | Some t when t.tokentype = Token.Mult ->
        let p, _ = expect p Token.Mult in
        let p, rhs = parse_multiplicative_expr p in
        (match rhs with
          | Some rhs_expr -> p, Some (NodeBinaryExpr (NodeBinExprMult { lhs = NodeTerm term; rhs = rhs_expr }))
          | None ->
            let _ = err "expected expression after '*'" in
            failwith "parser error")
      | _ -> p, Some (NodeTerm term))
  | _ -> p, None

and parse_stmt (p : parser_t) : parser_t * (node_stmt_t option) =
  let _ = print_endline "parse_stmt" in
  let _ = print_tokens p.tokens in
  match peek p with
  | Some t when t.tokentype = Token.Exit ->
     let p, _ = eat p in        (* eat exit *)
     let p, node_expr = parse_expr p in
     (match node_expr with
      | Some expr ->
         let p, _ = expect p Token.SemiColon in
         p, Some (NodeStmtExit expr)
      | None ->
         let _ = err "expected an expression after 'exit('" in
         failwith "parser error")
  | Some t when t.tokentype = Token.Println ->
     let p, _ = eat p in        (* eat println *)
     let p, node_expr = parse_expr p in
     (match node_expr with
      | Some expr ->
         let p, _ = expect p Token.SemiColon in
         p, Some (NodeStmtPrintln expr)
      | None ->
         let _ = err "expected an expression after 'println('" in
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
  | Some t when t.tokentype <> Token.EOF -> 
    let _ = err ("unrecognized token " ^ (Token.get_tokentype_as_str t.tokentype)) in
    failwith "parser error"
  | _ -> p, None (* no stmt found *)

let parse_program (p : parser_t) : node_prog_t =
  let _ = print_endline "parse_program" in
  let _ = print_tokens p.tokens in
  let rec parse_program' (p : parser_t) (program : node_prog_t) : parser_t * node_prog_t =
    let p', stmt = parse_stmt p in
    match stmt with
    | Some stmt -> parse_program' p' { stmts = program.stmts @ [stmt] }
    | None -> p', program
  in
  let _, result = parse_program' p { stmts = [] } in
  result
