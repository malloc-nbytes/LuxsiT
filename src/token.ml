type tokentype_t =
  (* Datatypes *)
  | Str
  | I32
  | U32

  (* Other. *)
  | Let
  | Ret
  | Colon
  | Proc
  | SemiColon
  | LParen
  | RParen
  | ID
  | StringLiteral
  | IntegerLiteral
  | Equals
  | Binop
  | EOF

type token_t =
  { data : string;
    tokentype : tokentype_t }

let token_create_wchar (data : char) (tokentype : tokentype_t) : token_t =
  { data = String.make 1 data; tokentype = tokentype }

let token_create_wstr (data : string) (tokentype : tokentype_t) : token_t =
  { data = data; tokentype = tokentype }

let token_print (token : token_t) : unit =
  match token.tokentype with
  | Let -> Printf.printf "Let: %s\n" token.data
  | Str -> Printf.printf "Str: %s\n" token.data
  | I32 -> Printf.printf "I32: %s\n" token.data
  | U32 -> Printf.printf "U32: %s\n" token.data
  | Colon -> Printf.printf "Colon: %s\n" token.data
  | SemiColon -> Printf.printf "SemiColon: %s\n" token.data
  | Proc -> Printf.printf "Proc: %s\n" token.data
  | Ret -> Printf.printf "Ret: %s\n" token.data
  | LParen -> Printf.printf "LParen: %s\n" token.data
  | RParen -> Printf.printf "RParen: %s\n" token.data
  | ID -> Printf.printf "ID: %s\n" token.data
  | IntegerLiteral -> Printf.printf "IntegerLiteral: %s\n" token.data
  | StringLiteral -> Printf.printf "StringLiteral: %s\n" token.data
  | Equals -> Printf.printf "Equals: %s\n" token.data
  | Binop -> Printf.printf "Binop: %s\n" token.data
  | EOF -> Printf.printf "EOF: %s\n" token.data
