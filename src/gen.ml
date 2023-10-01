open Parser

type var_t =
  { stackloc : int
  }

(* TODO: make `output` -> `section_text` *)
(* TODO: add `section_text *)
(* TODO: add `section_data *)
(* TODO: add `section_bss*)
(* TODO: add `section_rodata*)
(*
section .text
  - Purpose: This section is used for code instructions. It contains the actual
             executable machine code for your program.

  - Permissions: Typically, this section is marked as read-only and execute-only,
                 meaning you can't write to it, and it's meant for the CPU to execute
                 instructions from.

section .data
  - Purpose: This section is used for defining initialized data variables.
             These variables have values that are set at compile time.

  - Permissions: This section is usually marked as read-write.

section .bss
  - Purpose: This section is used for defining uninitialized data variables.
             These variables don't have an initial value; they are just allocated memory space.

  - Permissions: Like the .data section, it's usually marked as read-write.

section .rodata
  - Purpose: This section is used for read-only data, typically constants and strings.

  - Permissions: It is marked as read-only. *)
type gen_t =
  { output     : string
  ; stackptr   : int
  ; vars       : (string, var_t) Hashtbl.t list
  ; const_vars : (string, var_t) Hashtbl.t list
  }

let counter : int ref = ref 0

(* QAD solution for printing. *)
(* TODO: remove later. *)
(* label dump takes value in register rdi *)
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

let var_exists (gen : gen_t) (name : string) : bool =
  List.exists (fun table -> Hashtbl.mem table name) gen.vars || List.exists (fun table -> Hashtbl.mem table name) gen.const_vars

let const_var_exists (gen : gen_t) (name : string) : bool =
  List.exists (fun table -> Hashtbl.mem table name) gen.const_vars

let get_var (gen : gen_t) (name : string) : var_t option =
  let rec search_tables tables =
    match tables with
    | [] -> None
    | table :: rest ->
        if Hashtbl.mem table name then
          Some (Hashtbl.find table name)
        else
          search_tables rest
  in
  search_tables gen.vars

let get_mut_var (gen : gen_t) (name : string) : var_t option =
  let rec search_tables tables =
    match tables with
    | [] -> None
    | table :: rest ->
        if Hashtbl.mem table name then
          Some (Hashtbl.find table name)
        else
          search_tables rest
  in
  search_tables gen.vars

let get_const_var (gen : gen_t) (name : string) : var_t option =
  let rec search_tables tables =
    match tables with
    | [] -> None
    | table :: rest ->
        if Hashtbl.mem table name then
          Some (Hashtbl.find table name)
        else
          search_tables rest
  in
  search_tables gen.const_vars

let insert_var (gen : gen_t) (name : string) : unit =
  let v = { stackloc = gen.stackptr } in
  match gen.vars with
  | [] -> failwith "gen error: No variable symbol table on the stack"
  | table :: _ -> Hashtbl.add table name v

let insert_const_var (gen : gen_t) (name : string) : unit =
  let v = { stackloc = gen.stackptr } in
  match gen.const_vars with
  | [] -> failwith "gen error: No constant variable symbol table on the stack"
  | table :: _ -> Hashtbl.add table name v

let push_empty_var_table (gen : gen_t) : gen_t =
  let empty_table : (string, var_t) Hashtbl.t = Hashtbl.create 20 in
  { gen with vars = empty_table :: gen.vars }

let pop_var_table (gen : gen_t) : gen_t =
  match gen.vars with
  | [] -> failwith "gen error: No variable symbol table to pop"
  | _ :: rest -> { gen with vars = rest }

let push (gen : gen_t) (register : string) : gen_t =
  let output = gen.output in
  let output = output ^ "    push " ^ register ^ "\n" in
  { gen with output = output; stackptr = gen.stackptr + 1 }

let pop (gen : gen_t) (register : string) : gen_t =
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

(* TODO: refactor *)
(* TODO: <= and >= do not always function correct when (expr) <= (expr) or (expr) >= (expr). *)
let rec gen_expr (gen : gen_t) (expr : node_expr_t) : gen_t =
  match expr with
  | NodeTerm term -> gen_term gen term
  | NodeBinExpr bin_expr ->
      let gen = gen_expr gen bin_expr.lhs in
      let gen = gen_expr gen bin_expr.rhs in
      let gen = pop gen "rdi" in
      let gen = pop gen "rax" in
      let output = gen.output in
      match bin_expr.op with
      | "+" ->
        let output = output ^ "    add rax, rdi\n" in
        push { gen with output = output } "rax"
      | "-" ->
        let output = output ^ "    sub rax, rdi\n" in
        push { gen with output = output } "rax"
      | "*" ->
        let output = output ^ "    imul rax, rdi\n" in
        push { gen with output = output } "rax"
      | "/" ->
        let output = output ^ "    div rdi\n" in
        push { gen with output = output } "rax"
      | "==" ->
        let label_equal = "eq_" ^ string_of_int (!counter) in
        let label_not_equal = "neq_" ^ string_of_int (!counter) in
        let _ = counter := !counter + 1 in
        let output =
          output ^
          "    cmp rax, rdi\n" ^
          "    je " ^ label_equal ^ "\n" ^
          "    mov rax, 0\n" ^
          "    jmp " ^ label_not_equal ^ "\n" ^
          label_equal ^ ":\n" ^
          "    mov rax, 1\n" ^
          label_not_equal ^ ":\n"
        in
        push { gen with output = output } "rax"
      | "!=" ->
        let label_not_equal = "neq_" ^ string_of_int (!counter) in
        let label_equal = "eq_" ^ string_of_int (!counter) in
        let _ = counter := !counter + 1 in
        let output =
          output ^
          "    cmp rax, rdi\n" ^
          "    jne " ^ label_not_equal ^ "\n" ^
          "    mov rax, 0\n" ^
          "    jmp " ^ label_equal ^ "\n" ^
          label_not_equal ^ ":\n" ^
          "    mov rax, 1\n" ^
          label_equal ^ ":\n"
        in
        push { gen with output = output } "rax"
      | "<" ->
        let label_less = "less_" ^ string_of_int (!counter) in
        let label_not_less = "not_less_" ^ string_of_int (!counter) in
        let _ = counter := !counter + 1 in
        let output =
          output ^
          "    cmp rax, rdi\n" ^
          "    jl " ^ label_less ^ "\n" ^
          "    mov rax, 0\n" ^
          "    jmp " ^ label_not_less ^ "\n" ^
          label_less ^ ":\n" ^
          "    mov rax, 1\n" ^
          label_not_less ^ ":\n"
        in
        push { gen with output = output } "rax"
      | ">" ->
        let label_greater = "greater_" ^ string_of_int (!counter) in
        let label_not_greater = "not_greater_" ^ string_of_int (!counter) in
        let _ = counter := !counter + 1 in
        let output =
          output ^
          "    cmp rax, rdi\n" ^
          "    jg " ^ label_greater ^ "\n" ^
          "    mov rax, 0\n" ^
          "    jmp " ^ label_not_greater ^ "\n" ^
          label_greater ^ ":\n" ^
          "    mov rax, 1\n" ^
          label_not_greater ^ ":\n"
        in
        push { gen with output = output } "rax"
      | "<=" ->
        let label_less_equal = "less_equal_" ^ string_of_int (!counter) in
        let label_not_less_equal = "not_less_equal_" ^ string_of_int (!counter) in
        let _ = counter := !counter + 1 in
        let output =
          output ^
          "    cmp rax, rdi\n" ^
          "    jle " ^ label_less_equal ^ "\n" ^
          "    mov rax, 0\n" ^
          "    jmp " ^ label_not_less_equal ^ "\n" ^
          label_less_equal ^ ":\n" ^
          "    mov rax, 1\n" ^
          label_not_less_equal ^ ":\n"
        in
        push { gen with output = output } "rax"
      | ">=" ->
        let label_greater_equal = "greater_equal_" ^ string_of_int (!counter) in
        let label_not_greater_equal = "not_greater_equal_" ^ string_of_int (!counter) in
        let _ = counter := !counter + 1 in
        let output =
          output ^
          "    cmp rax, rdi\n" ^
          "    jge " ^ label_greater_equal ^ "\n" ^
          "    mov rax, 0\n" ^
          "    jmp " ^ label_not_greater_equal ^ "\n" ^
          label_greater_equal ^ ":\n" ^
          "    mov rax, 1\n" ^
          label_not_greater_equal ^ ":\n"
        in
        push { gen with output = output } "rax"
      | _ ->
        let _ = Err.err "gen error: unknown binary operator" in
        failwith "gen error"

let unwrap (a : 'a option) : 'a =
  match a with
  | Some a -> a
  | None -> failwith "gen error: unwrap failed"

(* TODO: change how variables are accessed. *)
(* We want to keep using the stack, however
   constantly copying can get expensive. *)
let rec generate_stmt (gen : gen_t) (stmt : Parser.node_stmt_t) : gen_t =
  match stmt with
  | NodeStmtExit stmt_exit ->
     let gen = gen_expr gen stmt_exit.expr in
     let output = gen.output in
     let output = output ^ "    mov rax, 60\n" in
     let gen = pop ({ gen with output = output }) "rdi" in
     { gen with output = gen.output ^ "    syscall\n" }
  | NodeStmtIf node_stmt_if ->
    let gen = push_empty_var_table gen in (* Start new scope. *)
    let true_branch_label = "if_true_" ^ string_of_int !counter in
    let end_label = "if_end_" ^ string_of_int !counter in
    let _ = counter := !counter + 1 in

    (* Generate code for the condition expression *)
    let gen = gen_expr gen node_stmt_if.expr in
    let gen = pop gen "rdi" in

    (* Compare the result with zero and jump to the true branch if true *)
    let output =
      gen.output ^
      "    cmp rdi, 0\n" ^
      "    je " ^ end_label ^ "\n" ^
      "    jmp " ^ true_branch_label ^ "\n" ^
      true_branch_label ^ ":\n"
    in

    let gen = { gen with output = output } in

    (* Generate code for the true branch *)
    let gen = iter_prog_stmts gen node_stmt_if.stmts in
  
    (* Unconditional jump to the end of the if statement *)
    let output = gen.output ^ "    jmp " ^ end_label ^ "\n" in
    let gen = { gen with output = output } in

    (* Generate code for the false branch (if it exists) *)
    let output = gen.output ^ end_label ^ ":\n" in
    let gen = pop_var_table gen in (* End scope. *)
    { gen with output = output }

  | NodeStmtWhile stmt_while ->
    let gen = push_empty_var_table gen in (* Start new scope. *)
    let start_label = "while_start_" ^ string_of_int !counter in
    let end_label = "while_end_" ^ string_of_int !counter in
    let _ = counter := !counter + 1 in

    (* Generate code for the condition expression *)
    let output = gen.output ^ start_label ^ ":\n" in
    let gen = { gen with output = output } in
    let gen = gen_expr gen stmt_while.expr in
    let gen = pop gen "rdi" in

    (* Compare the result with zero and jump to the end if true *)
    let output =
      gen.output ^
      "    cmp rdi, 0\n" ^
      "    je " ^ end_label ^ "\n"
    in

    let gen = { gen with output = output } in

    (* Generate code for the body *)
    let gen = iter_prog_stmts gen stmt_while.stmts in

    (* Unconditional jump to the start of the while loop *)
    let output = gen.output ^ "    jmp " ^ start_label ^ "\n" in
    let gen = { gen with output = output } in

    (* Generate code for the end of the while loop *)
    let output = gen.output ^ end_label ^ ":\n" in
    let gen = pop_var_table gen in (* End scope. *)
    { gen with output = output }

  | NodeStmtMutateVar stmt_mutate_var ->
      let _ = if const_var_exists gen stmt_mutate_var.id.data then
                let _ = Err.err ("cannot mutate constant variable " ^ stmt_mutate_var.id.data) in
                failwith "gen error" in
      let var : var_t =
        (match get_mut_var gen stmt_mutate_var.id.data with
         | Some var -> var
         | None ->
            let _ = Err.err ("undeclared ID " ^ stmt_mutate_var.id.data ^ "\n") in
            failwith "gen error") in
      let gen = gen_expr gen stmt_mutate_var.expr in
      let gen = pop gen "rdi" in
      let offset = string_of_int ((gen.stackptr - var.stackloc - 1) * 8) in
      let output = gen.output in
      let output = output ^ "    mov QWORD [rsp + " ^ offset ^ "], rdi\n" in
      { gen with output = output }
  | NodeStmtVarDecl stmt_var_decl ->
     (* Variable already exists. *)
     if var_exists gen stmt_var_decl.id.data then
       let _ = Err.err ("ID " ^ stmt_var_decl.id.data ^ " is already defined") in
       failwith "gen error"

     else
       (* Determine which hashtbl to put it in. *)
       let _ = if stmt_var_decl.constant then insert_const_var gen stmt_var_decl.id.data
               else insert_var gen stmt_var_decl.id.data in

       (* Initialized variable. *)
       if stmt_var_decl.expr <> None then
         gen_expr gen (unwrap stmt_var_decl.expr)

       (* Uninitialized variable. *)
       else
         let _ = Err.err "undedfined variables not yet implemented" in
         failwith "gen error"
  | NodeStmtPrintln stmt_print ->
     let gen = gen_expr gen stmt_print.expr in
     let gen = pop gen "rdi" in
     let output = gen.output ^ "    call dump\n" in
     { gen with output = output }

and iter_prog_stmts (gen : gen_t) (lst : node_stmt_t list) : gen_t =
  match lst with
  | [] -> gen
  | hd :: tl -> iter_prog_stmts (generate_stmt gen hd) tl

let generate_program (program : node_prog_t) : string =
  let gen = { output     = asm_header
            ; stackptr   = 0
            ; vars       = Hashtbl.create 20 :: []
            ; const_vars = Hashtbl.create 20 :: []
            } in

  let gen = iter_prog_stmts gen program.stmts in

  (* Obligatory exit for when the programmer forgets (ノ-_-)ノ ~┻━┻ *)

  let output = gen.output in
  let output = output ^ "    ; Obligatory exit\n" in
  let output = output ^ "    mov rax, 60\n" in
  let output = output ^ "    mov rdi, 0\n" in
  let output = output ^ "    syscall" in

  output
