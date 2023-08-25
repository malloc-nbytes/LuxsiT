(* https://www.youtube.com/watch?v=GOwk_pMlt1M&t=10s : 39:12 *)

type gen_t =
  { output : string;
    stackptr : int }

let generate_stmt (gen : gen_t) (stmt : Parser.node_stmt_t) : string =
  ""

let generate_program (program : Parser.node_prog_t) : string =
  let rec iter_prog_stmts (gen : gen_t) (lst : Parser.node_stmt_t list) : string =
    match lst with
    | [] -> gen.output
    | hd :: tl -> iter_prog_stmts gen tl
  in

  let gen = { output = "global _start\n_start:\n"; stackptr = 0 } in

  let output = iter_prog_stmts gen program.stmts in

  let output = gen.output in
  let output = output ^ "    mov rax, 60\n" in
  let output = output ^ "    mov rdi, 0" in
  let output = output ^ "    syscall" in
  output
