type tokentype_t =
  (* Datatypes *)
  | Str
  | I32

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
