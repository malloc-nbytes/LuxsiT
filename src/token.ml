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
  | While
  | Do
  | Then
  | If
  | End
  | Ret
  | Proc
  | Exit
  | Println
  | Const

  (* Symbols *)
  | RightArrow
  | GreaterThan
  | LessThan
  | Comma
  | Colon
  | DoubleColon
  | SemiColon
  | LParen
  | RParen
  | Assignment
  | Equality
  | Inequality
  | Plus
  | Hyphen
  | ForwardSlash
  | Asterisk
  | Bang
  | LessThanEqual
  | GreaterThanEqual

  (* Other *)
  | ID
  | StringLiteral
  | CharLiteral
  | IntegerLiteral
  | EOF

type token_t =
  { data : string;
    tokentype : tokentype_t }

(* Create a token from a character. *)
let token_create_wchar data tokentype : token_t =
  { data = String.make 1 data; tokentype = tokentype }

(* Create a token from a string. *)
let token_create_wstr data tokentype : token_t =
  { data = data; tokentype = tokentype }

(* For use in error messages. *)
let tokentype_tostr tokentype : string =
  match tokentype with
  (* Datatypes *)
  | Str -> "`str`"
  | I32 -> "`i32`"
  | U32 -> "`u32`"
  | Int -> "`int`"
  | Char -> "`char`"

  (* Keywords *)
  | Let -> "`let`"
  | While -> "`While`"
  | Do -> "`do`"
  | If -> "`if`"
  | Then -> "`then`"
  | Struct -> "`struct`"
  | End -> "`end`"
  | Proc -> "`proc`"
  | Ret -> "`ret`"
  | Exit -> "`exit`"
  | Println -> "`println`"
  | Const -> "`const`"

  (* Symbols *)
  | Colon -> "`:`"
  | DoubleColon -> "`::`"
  | SemiColon -> "`;`"
  | GreaterThan -> "`>`"
  | LessThan -> "`<`"
  | RightArrow -> "`->`"
  | LParen -> "`(`"
  | RParen -> "`)`"
  | Comma -> "`,`"
  | Plus -> "`+`"
  | Hyphen -> "`-`"
  | ForwardSlash -> "`/`"
  | Asterisk -> "`*`"
  | Assignment -> "`=`"
  | Equality -> "`==`"
  | Inequality -> "`!=`"
  | Bang -> "`!`"
  | LessThanEqual -> "`<=`"
  | GreaterThanEqual -> "`>=`"

  (* Other *)
  | ID -> "`ID`"
  | IntegerLiteral -> "`IntegerLiteral`"
  | StringLiteral -> "`StringLiteral`"
  | CharLiteral -> "`CharLiteral`"
  | EOF -> "`EOF`"
