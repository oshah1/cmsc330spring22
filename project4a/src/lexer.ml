open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
  let re_let = Str.regexp {|[ \n\r\x0c\t]*let|} in
  let re_add = Str.regexp {|[ \n\r\x0c\t]*\\+|} in
  let re_int = Str.regexp {|[ \n\r\x0c\t]*[0-9]+|-([0-9])|} in
  
  let re_bool = Str.regexp {|[ \n\r\x0c\t]*(true|false)|} in
  let re_id = Str.regexp {|[ \n\r\x0c\t]*[a-zA-Z][a-zA-Z0-9]*|} in
  
  let re_str = Str.regexp {|[ \n\r\x0c\t]*\"[^\"]*\"|} in
  let rec tok pos s =
    if pos >=String.length s then
      []
    else if (Str.string_match re_let s pos) then
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
          Tok_Let::(tok (pos + 3) s)
        end

      else if (Str.string_match re_add s pos) then
        (Tok_Add)::(tok (pos+1) s)

      else if (Str.string_match re_bool s pos) then
        let token = Str.matched_string s in
        let value = Str.string_match (Str.regexp "^(true)&") token 0 in
        (Tok_Bool value)::(tok (pos + (String.length token) -1) s)

      else if (Str.string_match re_int s pos) then
        let token = Str.matched_string s in
        let num = int_of_string (String.sub token 1 (String.length token) ) in
        (Tok_Int num)::(tok (pos + (String.length token) -1) s)

      else if (Str.string_match re_str s pos) then
        let token = Str.matched_string s in
        let len = String.length token in
        (Tok_String token)::(tok (pos + len - 1) s)

      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*<>|}) s pos) then
        (Tok_NotEqual)::(tok (pos + 2) s)
      
      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*>=|}) s pos) then
        
        (Tok_GreaterEqual)::(tok (pos + 2) s)

      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*<=|}) s pos) then
        
        (Tok_LessEqual)::(tok (pos + 2) s)

      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*->|}) s pos) then
        (Tok_Arrow)::(tok (pos+2) s)
      
      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*>|}) s pos) then
        
        (Tok_Greater)::(tok (pos + 1) s)

      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*>|}) s pos) then
        
        (Tok_Less)::(tok (pos+1) s)

      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*=|}) s pos) then
        
        (Tok_Equal)::(tok (pos+1) s)

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*-") s pos) then
        (Tok_Sub)::(tok (pos+1) s)

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]**") s pos) then
        (Tok_Mult)::(tok (pos+1) s)

      else if (Str.string_match (Str.regexp "\\/") s pos) then
        (Tok_Div)::(tok (pos+1) s)

      else if (Str.string_match (Str.regexp "\\(if\\)") s pos) then
        (Tok_If)::(tok (pos+4) s)

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*then") s pos) then
        (Tok_Then)::(tok (pos+6) s)

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*else") s pos) then
        (Tok_Else)::(tok (pos+6) s)

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*fun") s pos) then
        (Tok_Fun)::(tok (pos+5) s)

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*rec") s pos) then
        (Tok_Rec)::(tok (pos+5) s)

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*def") s pos) then
        (Tok_Def)::(tok (pos+5) s)

      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*not|}) s pos) then
        (Tok_Not)::(tok (pos + (String.length (Str.matched_string s)) - 1) s)

      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*/|{2}|}) s pos) then
        (Tok_Or)::(tok (pos + (String.length (Str.matched_string s)) -1) s)

      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*\&\&|}) s pos) then
        (Tok_And)::(tok (pos+ (String.length (Str.matched_string s)) - 1) s) 

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*in") s pos) then
        (Tok_In)::(tok (pos+4) s)

      else if (Str.string_match re_id s pos) then
        let token = Str.matched_string s in
        (Tok_ID token)::(tok (pos + (String.length token) -1) s)

      else if (Str.string_match (Str.regexp ";;") s pos) then
        (Tok_DoubleSemi)::(tok (pos+2) s)

      else if (Str.string_match (Str.regexp "\\(") s pos) then
        (Tok_LParen)::(tok (pos+1) s)

      else if (Str.string_match (Str.regexp "\\)") s pos) then
        (Tok_RParen)::(tok (pos+1) s)

      else
        tok (pos+1) s

      in tok 0 input


