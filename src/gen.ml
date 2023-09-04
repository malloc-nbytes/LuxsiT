open Parser

type var_t =
  { stackloc : int }

type gen_t =
  { output : string;
    stackptr : int;
    vars : (string, var_t) Hashtbl.t }

(* QAD solution for printing. *)
(* TODO: remove later. *)
let asm_header =
  "section .text\n" ^
  "dump:\n" ^
  "    mov     r8, -3689348814741910323\n" ^
  "    sub     rsp, 40\n" ^
  "    mov     BYTE [rsp+31], 10\n" ^
  "    lea     rcx, [rsp+30]\n" ^
  ".L2:\n" ^
  "    mov     rax, rdi\n" ^
  "    mul     r8\n" ^
  "    mov     rax, rdi\n" ^
  "    shr     rdx, 3\n" ^
  "    lea     rsi, [rdx+rdx*4]\n" ^
  "    add     rsi, rsi\n" ^
  "    sub     rax, rsi\n" ^
  "    mov     rsi, rcx\n" ^
  "    sub     rcx, 1\n" ^
  "    add     eax, 48\n" ^
  "    mov     BYTE [rcx+1], al\n" ^
  "    mov     rax, rdi\n" ^
  "    mov     rdi, rdx\n" ^
  "    cmp     rax, 9\n" ^
  "    ja      .L2\n" ^
  "    lea     rdx, [rsp+32]\n" ^
  "    mov     edi, 1\n" ^
  "    sub     rdx, rsi\n" ^
  "    mov     rax, 1\n" ^
  "    syscall\n" ^
  "    add     rsp, 40\n" ^
  "    ret\n" ^
  "global _start\n" ^
  "_start:\n"

let var_exists gen name : bool =
  Hashtbl.mem gen.vars name

let get_var gen name : var_t option =
  if var_exists gen name then
    let var = Hashtbl.find gen.vars name in
    Some var
  else
    None

let insert_var gen name : unit =
  let v = { stackloc = gen.stackptr } in
  Hashtbl.add gen.vars name v

let push gen register : gen_t =
  let output = gen.output in
  let output = output ^ "    push " ^ register ^ "\n" in
  { gen with output = output; stackptr = gen.stackptr + 1 }

let pop gen register : gen_t =
  let output = gen.output in
  let output = output ^ "    pop " ^ register ^ "\n" in
  { gen with output = output; stackptr = gen.stackptr - 1 }

let gen_term (gen : gen_t) (term : node_term_t) : gen_t =
  match term with
  | NodeTermIntLit term_intlit ->
     let output = gen.output in
     let output = output ^ "    mov rax, " ^ term_intlit.intlit.data ^ "\n" in
     push ({ gen with output = output }) "rax"
  | NodeTermID term_id ->
     let var : var_t =
       (match get_var gen term_id.id.data with
        | Some var -> var
        | None ->
           let _ = Err.err ("undeclared ID " ^ term_id.id.data ^ "\n") in
           failwith "gen error") in
     let offset = string_of_int ((gen.stackptr - var.stackloc - 1) * 8) in
     let output = gen.output ^ "    mov rax, QWORD [rsp + " ^ offset ^ "]\n" in
     push { gen with output = output } "rax"

let rec gen_expr (gen : gen_t) (expr : node_expr_t) : gen_t =
  match expr with
  | NodeTerm term -> gen_term gen term
  | NodeBinExpr bin_expr ->
     (match bin_expr.op with
      | "+" ->
         let gen = gen_expr gen bin_expr.lhs in
         let gen = gen_expr gen bin_expr.rhs in
         let gen = pop gen "rdi" in
         let gen = pop gen "rax" in
         let output = gen.output in
         let output = output ^ "    add rax, rdi\n" in
         push { gen with output = output } "rax"
      | "-" ->
         let gen = gen_expr gen bin_expr.lhs in
         let gen = gen_expr gen bin_expr.rhs in
         let gen = pop gen "rdi" in
         let gen = pop gen "rax" in
         let output = gen.output in
         let output = output ^ "    sub rax, rdi\n" in
         push { gen with output = output } "rax"
      | "*" ->
         let gen = gen_expr gen bin_expr.lhs in
         let gen = gen_expr gen bin_expr.rhs in
         let gen = pop gen "rdi" in
         let gen = pop gen "rax" in
         let output = gen.output in
         let output = output ^ "    imul rax, rdi\n" in
         push { gen with output = output } "rax"
      | "/" ->
         let gen = gen_expr gen bin_expr.lhs in
         let gen = gen_expr gen bin_expr.rhs in
         let gen = pop gen "rdi" in
         let gen = pop gen "rax" in
         let output = gen.output in
         let output = output ^ "    div rdi\n" in
         push { gen with output = output } "rax"
      | _ -> failwith "gen error: unknown binary operator")

let unwrap (a : 'a option) : 'a =
  match a with
  | Some a -> a
  | None -> failwith "gen error: unwrap failed"

let generate_stmt gen stmt : gen_t =
  match stmt with
  | NodeStmtExit stmt_exit ->
     let gen = gen_expr gen stmt_exit.expr in
     let output = gen.output in
     let output = output ^ "    mov rax, 60\n" in
     let gen = pop ({ gen with output = output }) "rdi" in
     { gen with output = gen.output ^ "    syscall\n" }
  | NodeStmtVarDecl stmt_var_decl ->
     if var_exists gen stmt_var_decl.id.data then
       let _ = Err.err ("ID " ^ stmt_var_decl.id.data ^ " is already defined") in
       failwith "gen error"
     else
       let _ = insert_var gen stmt_var_decl.id.data in
       if stmt_var_decl.expr <> None then
        let _ = if stmt_var_decl.constant then failwith "gen: constants not implemented" in
        gen_expr gen @@ unwrap stmt_var_decl.expr
       else
        failwith "undefined variables not implemented"
  | NodeStmtPrintln stmt_print ->
     let gen = gen_expr gen stmt_print.expr in
     let gen = pop gen "rdi" in
     let output = gen.output ^ "    call dump\n" in
     { gen with output = output }

let generate_program (program : node_prog_t) : string =
  let rec iter_prog_stmts (gen : gen_t) (lst : node_stmt_t list) : gen_t =
    match lst with
    | [] -> gen
    | hd :: tl -> iter_prog_stmts (generate_stmt gen hd) tl
  in

  let gen = { output = asm_header;
              stackptr = 0;
              vars = Hashtbl.create 20 } in

  let gen = iter_prog_stmts gen program.stmts in

  (* Obligatory exit for when the programmer forgets (ノ-_-)ノ ~┻━┻ *)

  let output = gen.output in
  let output = output ^ "    ; Obligatory exit\n" in
  let output = output ^ "    mov rax, 60\n" in
  let output = output ^ "    mov rdi, 0\n" in
  let output = output ^ "    syscall" in

  output
