type tokentype_t =
  (* Datatypes *)
  | Str
  | I32
  | Int
  | U32
  | Char
  | Struct

  (* Keywords *)
  | Let
  | Then
  | If
  | End
  | Ret
  | Proc
  | Exit
  | Println

  (* Symbols *)
  | RightArrow
  | GreaterThan
  | LessThan
  | Comma
  | Colon
  | SemiColon
  | LParen
  | RParen
  | Equals
  | Binop
  | Plus
  | Mult

  (* Other *)
  | ID
  | StringLiteral
  | CharLiteral
  | IntegerLiteral
  | EOF

type token_t =
  { data : string;
    tokentype : tokentype_t }

let token_create_wchar (data : char) (tokentype : tokentype_t) : token_t =
  { data = String.make 1 data; tokentype = tokentype }

let token_create_wstr (data : string) (tokentype : tokentype_t) : token_t =
  { data = data; tokentype = tokentype }

(* For use in error messages. *)
let get_tokentype_as_str (tokentype : tokentype_t) : string =
  match tokentype with
  | Let -> "`let`"
  | If -> "`if`"
  | Then -> "`then`"
  | Str -> "`str`"
  | I32 -> "`i32`"
  | U32 -> "`u32`"
  | Int -> "`int`"
  | Char -> "`char`"
  | Struct -> "`struct`"
  | Colon -> "`:`"
  | SemiColon -> "`;`"
  | GreaterThan -> "`>`"
  | LessThan -> "`<`"
  | RightArrow -> "`->`"
  | End -> "`end`"
  | Proc -> "`proc`"
  | Ret -> "`ret`"
  | Exit -> "`exit`"
  | Println -> "`println`"
  | LParen -> "`(`"
  | RParen -> "`)`"
  | Comma -> "`,`"
  | Plus -> "`+`"
  | Mult -> "`*`"
  | ID -> "`ID`"
  | IntegerLiteral -> "`IntegerLiteral`"
  | StringLiteral -> "`StringLiteral`"
  | CharLiteral -> "`CharLiteral`"
  | Equals -> "`=`"
  | Binop -> "`Binop`"
  | EOF -> "`EOF`"


(* Debugging printing tokens. *)
let token_print (token : token_t) : unit =
  match token.tokentype with
  | Let -> Printf.printf "Let\n"
  | If -> Printf.printf "If\n"
  | Then -> Printf.printf "Then\n"
  | Str -> Printf.printf "Str\n"
  | I32 -> Printf.printf "I32\n"
  | U32 -> Printf.printf "U32\n"
  | Int -> Printf.printf "Int\n"
  | Char -> Printf.printf "Char\n"
  | Struct -> Printf.printf "Struct\n"
  | Colon -> Printf.printf "Colon\n"
  | SemiColon -> Printf.printf "SemiColon\n"
  | GreaterThan -> Printf.printf "GreaterThan\n"
  | RightArrow -> Printf.printf "RightArrow\n"
  | LessThan -> Printf.printf "LessThan\n"
  | End -> Printf.printf "End\n"
  | Exit -> Printf.printf "Exit\n"
  | Println -> Printf.printf "Println\n"
  | Proc -> Printf.printf "Proc\n"
  | Ret -> Printf.printf "Ret\n"
  | LParen -> Printf.printf "LParen\n"
  | RParen -> Printf.printf "RParen\n"
  | Comma -> Printf.printf "Comma\n"
  | Plus -> Printf.printf "Plus\n"
  | Mult -> Printf.printf "Mult\n"
  | ID -> Printf.printf "ID: %s\n" token.data
  | IntegerLiteral -> Printf.printf "IntegerLiteral: %s\n" token.data
  | StringLiteral -> Printf.printf "StringLiteral: %s\n" token.data
  | CharLiteral -> Printf.printf "CharLiteral: %s\n" token.data
  | Equals -> Printf.printf "Equals\n"
  | Binop -> Printf.printf "Binop: %s\n" token.data
  | EOF -> Printf.printf "EOF\n"
