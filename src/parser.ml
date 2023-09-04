type stmt_t =
  | ExprStmt of expr_t

and expr_t =
  | ID of identifier_t
  | IntegerLiteral of integer_literal_t
  | BinaryExpr of binary_expr_t
  | Let of let_t
  | Exit of exit_t

and program_t =
  { body : stmt_t list }

and binary_expr_t =
  { left : expr_t
  ; right : expr_t
  ; operator : string }

and identifier_t =
  { op : string }

and integer_literal_t =
  { value : string }

and let_t =
  { id : identifier_t
  ; expr : expr_t }

and exit_t =
  { expr : expr_t }

type parser_t =
  { tokens : Token.token_t list }

let err (msg : string) : unit =
  print_endline msg


let expect (p : parser_t) (expected_type : Token.tokentype_t) : parser_t * Token.token_t =
  match p.tokens with
  | [] -> failwith "no more tokens"
  | hd :: tl when hd.tokentype = expected_type -> { tokens = tl }, hd
  | hd :: _ ->
    let _ = err ("expected token " ^ (Token.get_tokentype_as_str expected_type) ^ " but got " ^ hd.data) in 
    failwith "expected token"


let eat (p : parser_t) : parser_t * Token.token_t =
  match p.tokens with
  | [] -> failwith "no more tokens"
  | hd :: tl -> { tokens = tl }, hd


let at (p : parser_t) : Token.token_t =
  match p.tokens with
  | [] -> failwith "no more tokens"
  | hd :: _ -> hd


let rec parse_primary_expr (p : parser_t) : parser_t * expr_t =
  match at p with
  | t when t.tokentype = Token.Let ->
    let p, _ = eat p in
    let p, id = expect p Token.ID in
    let p, _ = expect p Token.Equals in
    let p, expr = parse_expr p in
    let p, _ = expect p Token.SemiColon in
    p, Let { id = { op = id.data }; expr }
  | t when t.tokentype = Token.Exit ->
    let p, _ = eat p in
    let p, expr = parse_expr p in
    let p, _ = expect p Token.SemiColon in
    p, Exit { expr }
  | t when t.tokentype = Token.ID ->
    let p, t = eat p in
    p, ID { op = t.data }
  | t when t.tokentype = Token.IntegerLiteral ->
    let p, t = eat p in
    p, IntegerLiteral { value = t.data }
  | t when t.tokentype = Token.LParen ->
    let p, _ = eat p in
    let p, expr = parse_expr p in
    let p, _ = expect p Token.RParen in
    p, expr
  | _ -> let _ = err ("unexpected token " ^ (at p).data) in failwith "unexpected token"


and parse_multiplicitave_expr (p : parser_t) : parser_t * expr_t =
  let p, lhs = parse_primary_expr p in
  let rec parse_multiplicitave_expr (p : parser_t) (lhs : expr_t) : parser_t * expr_t =
    match at p with
    | t when t.tokentype = Token.Mult ->
      let p, t = eat p in
      let p, rhs = parse_primary_expr p in
      parse_multiplicitave_expr p (BinaryExpr { left = lhs; right = rhs; operator = t.data })
    | _ -> p, lhs in
  parse_multiplicitave_expr p lhs


and parse_additive_expr (p : parser_t) : parser_t * expr_t =
  let p, lhs = parse_multiplicitave_expr p in
  let rec parse_additive_expr (p : parser_t) (lhs : expr_t) : parser_t * expr_t =
    match at p with
    | t when t.tokentype = Token.Plus ->
      let p, t = eat p in
      let p, rhs = parse_multiplicitave_expr p in
      parse_additive_expr p (BinaryExpr { left = lhs; right = rhs; operator = t.data })
    | _ -> p, lhs in
  parse_additive_expr p lhs


and parse_expr (p : parser_t) : parser_t * expr_t =
  let p, expr = parse_additive_expr p in
  p, expr


let parse_stmt (p : parser_t) : parser_t * stmt_t =
  let p, expr = parse_expr p in
  p, ExprStmt expr


(* Entrypoint. *)
let produce_ast (tokens : Token.token_t list) : program_t =
  let p = { tokens } in
  let rec produce_program (p : parser_t) (program : program_t) : program_t =
    match p.tokens with
    | [] -> program
    | hd :: _ when hd.tokentype = Token.EOF -> program
    | _ -> 
      let p, stmt = parse_stmt p in
      produce_program p { body = program.body @ [stmt] }
  in
  produce_program p { body = [] }


(* Debugging. *)
let rec print_expr (expr : expr_t) (indent : int) : unit =
  let indentation = String.make (2 * indent) ' ' in
  match expr with
  | ID id ->
    Printf.printf "%sID: %s\n" indentation id.op
  | IntegerLiteral lit ->
    Printf.printf "%sIntegerLiteral: %s\n" indentation lit.value
  | BinaryExpr bin_expr ->
    Printf.printf "%sBinaryExpr:\n" indentation;
    Printf.printf "%s  Operator: %s\n" indentation bin_expr.operator;
    Printf.printf "%s  Left:\n" indentation;
    print_expr bin_expr.left (indent + 1);
    Printf.printf "%s  Right:\n" indentation;
    print_expr bin_expr.right (indent + 1)
  | Let let_expr ->
    Printf.printf "%sLet:\n" indentation;
    Printf.printf "%s  ID: %s\n" indentation let_expr.id.op;
    Printf.printf "%s  Expr:\n" indentation;
    print_expr let_expr.expr (indent + 1)
  | Exit exit_expr ->
    Printf.printf "%sExit:\n" indentation;
    Printf.printf "%s  Expr:\n" indentation;
    print_expr exit_expr.expr (indent + 1)


let rec print_stmt (stmt : stmt_t) (indent : int) : unit =
  let indentation = String.make (2 * indent) ' ' in
  match stmt with
  | ExprStmt expr ->
    Printf.printf "%sExprStmt:\n" indentation;
    print_expr expr (indent + 1)


let rec print_program (program : program_t) (indent : int) : unit =
  let indentation = String.make (2 * indent) ' ' in
  Printf.printf "%sProgram:\n" indentation;
  List.iter (fun stmt -> print_stmt stmt (indent + 1)) program.body


let print_program_structure (program : program_t) : unit =
  print_program program 0


let () =
  let lexer = Lexer.parse_code "let tmp = 1 + (2 * 3);" in
  let program = produce_ast lexer.tokens in
  print_program_structure program