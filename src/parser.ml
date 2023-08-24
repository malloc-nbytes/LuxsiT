type parser_t =
  { tokens: Token.token_t list }

let parser_create (tokens : Token.token_t list) : parser_t =
  { tokens = tokens }

let parse (p : parser_t) : unit =
  ()

let eat (p : parser_t) : parser_t * Token.token_t =
  parser_create (List.tl p.tokens), List.hd p.tokens

let expect (p : parser_t) (expected_type : Token.tokentype_t) : parser_t * Token.token_t =
  let hd = List.hd p.tokens and tl = List.tl p.tokens in
  if hd.tokentype <> expected_type then
    failwith (Printf.sprintf "PARSER ERR: unexpected token: %s\n" hd.data)
  else
    parser_create tl, hd


