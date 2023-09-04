type node_stmt_t =
  | NodeStmtExit of node_stmt_exit_t
  | NodeStmtLet of node_stmt_let_t
  | NodeStmtPrintln of node_stmt_println_t

and node_term_t =
  | NodeTermID of node_term_id_t
  | NodeTermIntLit of node_term_intlit

and node_expr_t =
  | NodeBinExpr of node_bin_expr_t
  | NodeTerm of node_term_t

and node_bin_expr_t =
  { lhs : node_expr_t 
  ; rhs : node_expr_t
  ; op : string }

and node_prog_t =
  { stmts : node_stmt_t list }

and node_term_id_t =
  { id : Token.token_t }

and node_term_intlit =
  { intlit : Token.token_t }

and node_stmt_exit_t =
  { expr : node_expr_t }

and node_stmt_let_t =
  { id : Token.token_t
  ; expr : node_expr_t }

and node_stmt_println_t =
  { expr : node_expr_t }
