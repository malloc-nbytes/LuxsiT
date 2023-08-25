type tokentype_t =
  (* Datatypes *)
  | Str
  | I32
  | Int
  | U32
  | Char
  | Struct

  (* Other. *)
  | Let
  | End
  | GreaterThan
  | LessThan
  | Comma
  | Ret
  | Colon
  | Proc
  | SemiColon
  | LParen
  | RParen
  | ID
  | StringLiteral
  | CharLiteral
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

let get_tokentype_as_str (tokentype : tokentype_t) : string =
  match tokentype with
  | Let -> "Let"
  | Str -> "Str"
  | I32 -> "I32"
  | U32 -> "U32"
  | Int -> "Int"
  | Char -> "Char"
  | Struct -> "Struct"
  | Colon -> "Colon"
  | SemiColon -> "SemiColon"
  | GreaterThan -> "GreaterThan"
  | LessThan -> "LessThan"
  | End -> "End"
  | Proc -> "Proc"
  | Ret -> "Ret"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | Comma -> "Comma"
  | ID -> "ID"
  | IntegerLiteral -> "IntegerLiteral"
  | StringLiteral -> "StringLiteral"
  | CharLiteral -> "CharLiteral"
  | Equals -> "Equals"
  | Binop -> "Binop"
  | EOF -> "EOF"


let token_print (token : token_t) : unit =
  match token.tokentype with
  | Let -> Printf.printf "Let\n"
  | Str -> Printf.printf "Str\n"
  | I32 -> Printf.printf "I32\n"
  | U32 -> Printf.printf "U32\n"
  | Int -> Printf.printf "Int\n"
  | Char -> Printf.printf "Char\n"
  | Struct -> Printf.printf "Struct\n"
  | Colon -> Printf.printf "Colon\n"
  | SemiColon -> Printf.printf "SemiColon\n"
  | GreaterThan -> Printf.printf "GreaterThan\n"
  | LessThan -> Printf.printf "LessThan\n"
  | End -> Printf.printf "End\n"
  | Proc -> Printf.printf "Proc\n"
  | Ret -> Printf.printf "Ret\n"
  | LParen -> Printf.printf "LParen\n"
  | RParen -> Printf.printf "RParen\n"
  | Comma -> Printf.printf "Comma\n"
  | ID -> Printf.printf "ID: %s\n" token.data
  | IntegerLiteral -> Printf.printf "IntegerLiteral: %s\n" token.data
  | StringLiteral -> Printf.printf "StringLiteral: %s\n" token.data
  | CharLiteral -> Printf.printf "CharLiteral: %s\n" token.data
  | Equals -> Printf.printf "Equals\n"
  | Binop -> Printf.printf "Binop: %s\n" token.data
  | EOF -> Printf.printf "EOF\n"
