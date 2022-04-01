
(* Type *)
type token =
| Tok_Int of int
| Tok_Mult
| Tok_Plus
| Tok_LParen
| Tok_RParen
| Tok_EOF

let string_of_token tok = match tok with
| Tok_Int(i) -> string_of_int i
| Tok_Mult -> "*"
| Tok_Plus -> "+"
| Tok_LParen -> "("
| Tok_RParen -> ")"
| Tok_EOF -> ""

let rec string_of_list conv lst = 
  match lst with
  | [] -> ""
  | h::[] -> conv h
  | h::t -> (conv h) ^ " " ^ (string_of_list conv t)


let rec lexer (input : string) : token list = 
  let rec tok pos =

    if pos >= String.length input then [Tok_EOF]

    else if Str.string_match (Str.regexp "(") input pos then
      Tok_LParen :: (tok (pos + 1))

    else if Str.string_match (Str.regexp ")") input pos then
      Tok_RParen :: (tok (pos + 1))

    else if Str.string_match (Str.regexp "\\+") input pos then
      Tok_Plus :: (tok (pos+1))

    else if Str.string_match (Str.regexp "\\*") input pos then
      Tok_Mult :: (tok (pos+1))

    else if Str.string_match (Str.regexp "-?[0-9]+") input pos then
      let value = Str.matched_string input input
      Tok_Int(int_of_string value) :: (tok (pos+1))
    
    else
      tok (pos+1)

    in tok 0