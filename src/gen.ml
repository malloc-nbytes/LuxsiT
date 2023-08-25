(* https://www.youtube.com/watch?v=GOwk_pMlt1M&t=10s : 39:12 *)

type gen_t =
  { output : string;
    stackptr : int }

let err (msg : string) : unit =
  Printf.printf "(ERR) %s\n" msg

let generate_stmt (gen : gen_t) (stmt : Parser.node_stmt_t) : gen_t =
  match stmt with
  | NodeStmtExit expr -> failwith "unimplemented"
  | NodeStmtLet stmt_let -> failwith "unimplemented"

let generate_program (program : Parser.node_prog_t) : string =
  let rec iter_prog_stmts (gen : gen_t) (lst : Parser.node_stmt_t list) : gen_t =
    match lst with
    | [] -> gen
    | hd :: tl -> iter_prog_stmts (generate_stmt gen hd) tl
  in

  let gen = { output = "global _start\n_start:\n"; stackptr = 0 } in

  let gen = iter_prog_stmts gen program.stmts in

  let output = gen.output in
  let output = output ^ "    mov rax, 60\n" in
  let output = output ^ "    mov rdi, 0" in
  let output = output ^ "    syscall" in
  
  output