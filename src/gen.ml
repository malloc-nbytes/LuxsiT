type var_t =
  { stackloc : int }

type gen_t =
  { output : string;
    stackptr : int;
    vars : (string, var_t) Hashtbl.t }

let err (msg : string) : unit =
  Printf.printf "(ERR) %s\n" msg

let var_exists (gen : gen_t) (name : string) : bool =
  Hashtbl.mem gen.vars name

let get_var (gen : gen_t) (name : string) : var_t option =
  if var_exists gen name then
    let var = Hashtbl.find gen.vars name in
    Some var
  else
    None

let insert_var (gen : gen_t) (name : string) : unit =
  let v = { stackloc = gen.stackptr } in
  Hashtbl.add gen.vars name v

let push (gen : gen_t) (reg : string) : gen_t =
  let output = gen.output in
  let output = output ^ "    push " ^ reg ^ "\n" in
  { gen with output = output; stackptr = gen.stackptr + 1 }

let pop (gen : gen_t) (reg : string) : gen_t =
  let output = gen.output in
  let output = output ^ "    pop " ^ reg ^ "\n" in
  { gen with output = output; stackptr = gen.stackptr - 1 }

let gen_expr (gen : gen_t) (expr : Parser.node_expr_t) : gen_t =
  match expr with
  | Parser.NodeExprIntlit expr_intlit -> 
    let output = gen.output in
    let output = output ^ "    mov rax, " ^ expr_intlit.intlit.data ^ "\n" in
    push ({ gen with output = output }) "rax"
  | Parser.NodeExprId expr_id ->

    let var : var_t =
      match get_var gen expr_id.id.data with
      | Some var -> var
      | None ->
         let _ = err ("undeclared ID " ^ expr_id.id.data ^ "\n") in
         failwith "gen error" in

    let offset = string_of_int ((gen.stackptr - var.stackloc - 1) * 8) in
    let output = gen.output ^ "    mov rax, QWORD [rsp + " ^ offset ^ "]\n" in
    let gen = push { gen with output = output } "rax" in
    gen
  | Parser.NodeBinaryExpr bin_expr ->
     failwith "unimplemented"

let generate_stmt (gen : gen_t) (stmt : Parser.node_stmt_t) : gen_t =
  match stmt with
  | Parser.NodeStmtExit stmt_exit ->
    let gen = gen_expr gen stmt_exit in
    let output = gen.output in
    let output = output ^ "    mov rax, 60\n" in
    let gen = pop ({ gen with output = output }) "rdi" in
    { gen with output = gen.output ^ "    syscall\n" }
  | Parser.NodeStmtLet stmt_let ->
     if var_exists gen stmt_let.id.data then
       let _ = err ("ID " ^ stmt_let.id.data ^ " is already defined") in
       failwith "gen error"
     else
       let _ = insert_var gen stmt_let.id.data in
       gen_expr gen stmt_let.expr

let generate_program (program : Parser.node_prog_t) : string =
  let rec iter_prog_stmts (gen : gen_t) (lst : Parser.node_stmt_t list) : gen_t =
    match lst with
    | [] -> gen
    | hd :: tl -> iter_prog_stmts (generate_stmt gen hd) tl
  in

  let gen = { output = "global _start\n_start:\n";
              stackptr = 0;
              vars = Hashtbl.create 20 } in

  let gen = iter_prog_stmts gen program.stmts in

  (* Obligatory exit for when the programmer forgets (>д<) *)

  let output = gen.output in
  let output = output ^ "    mov rax, 60\n" in
  let output = output ^ "    mov rdi, 0\n" in
  let output = output ^ "    syscall" in

  output
