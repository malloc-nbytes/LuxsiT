type gen_t =
  { root : Parser.node_ret_t;
    output : string }

let gen_create (root : Parser.node_ret_t) : gen_t =
  { root = root; output = "" }

let generate (gen : gen_t) : string =
  let output = "global _start\n_start:\n" in
  let output = output ^ "    mov rax, 60\n" in
  let output = output ^ "    mov rdi, " ^ gen.root.expr.intlit.data ^ "\n" in
  let output = output ^ "    syscall" in
  output
