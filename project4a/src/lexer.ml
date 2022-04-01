open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
  let re_let = Str.regexp "\s*let" in
  let re_add = "\s*+" in
  let re_int = "\s*([0-9]+|-\([0-9]+\))" in
  let re_eq = "\s*=" in
  let re_bool = "\s*(true|false)" in
  let re_id = "[a-zA-Z][a-zA-Z0-9]*" in
  let re_lp = "\s*\(" in
  let rec tok pos s =
    if pos >=String.length s then
      []
    else
    if (Str.string_match re_let s pos) then
      let token = Str.matched_string s in
      (*decide if match to let or id is longer*)
        begin
        if (Str.string_match re_id s pos) then
          let token2 = Str.matched_string s in
          begin 
          if String.length token2 > String.length token then
            (Tok_ID token2)::(tok (pos + (String.length token2) -1) s)
          else
            Tok_Let::(tok (pos + (String.length token) - 1) s)
          end
        else
          Tok_Let
        end
      else if (Str.string_match re_id s pos) then
        let token = Str.matched_string in
        (Tok_ID token)::(tok (pos + (String.length token) -1) s)
else if (Str.string_match re_add s pos) then
  (Tok_Add)::(tok (pos+1) s)
else if (Str.string_match re_bool s pos) then
  token = Str.matched_string in
  let value = Str.string_match "^(true)&" token 0 in
  (Tok_Bool value)::(tok (pos + (String.length token) -1) s)
else if (Str.string_match re_int s pos) then
  let num = string_of_int (matched_string) in
  (Tok_Int num)::(tok (pos + (String.length matched_string) -1) s)

