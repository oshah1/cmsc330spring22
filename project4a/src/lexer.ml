open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input =
  
  let re_bool = Str.regexp {|[ \n\r\x0c\t]*(true\|false)|} in
  let re_id = Str.regexp {|[ \n\r\x0c\t]*[a-zA-Z][a-zA-Z0-9]*|} in
  
  let re_str = Str.regexp {|[ \n\r\x0c\t]*\"[^\"]*\"|} in

  let len = String.length input in
  let rec tok pos =
    if pos >= len then
      []
    else if (Str.string_match re_id input pos) then
      let token = Str.matched_string input in
      let length = String.length token in
      (*see if token matches with any other token*)
        begin
        if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*\"[^\"]*if") input pos) then
          let token2 = Str.matched_string input in
          begin
          if length > (String.length token2) then
            (Tok_ID token)::(tok (pos+length))
          else
            (Tok_If)::(tok (pos+(String.length token2)))
          end
          else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*\"[^\"]*else") input pos) then
            let tok3 = Str.matched_string input in
            let len3 =String.length tok3 in 
            if length > (len3) then
              (Tok_ID token)::(tok (pos+length))
            else
              (Tok_Else)::(tok (pos+len3))
          else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*\"[^\"]*then") input pos) then
              let tok4 = Str.matched_string input in
              let len4 = String.length tok4 in
              if length > len4 then
                (Tok_ID token)::(tok (pos +length))
              else
                (Tok_Then)::(tok (pos+len4))

          else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*let")input pos) then
              let tok5 = Str.matched_string input in
              let len5 = String.length tok5 in
              if length <= len5 then
                (Tok_Let)::(tok (pos+len5))
              else
                (Tok_ID token)::(tok (pos+length))
          else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*not") input pos) then
            let tok6 = Str.matched_string input in
            let len6 = String.length tok6 in
            if length <= (String.length tok6) then
              (Tok_Not)::(tok (pos+len6))
            else
              (Tok_ID token)::(tok (pos +length))

          else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*def") input pos) then
            let tok7 = Str.matched_string input in
            let len7 = String.length tok7 in
            if length <= len7 then
              (Tok_Def)::(tok (pos+len7))
            else
              (Tok_ID token)::(tok (pos +length))

          else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*rec") input pos) then
            let tok7 = Str.matched_string input in
            if length <= (String.length tok7) then
              (Tok_Rec)::(tok (pos+3))
            else
              (Tok_ID token)::(tok (pos +length))

          else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*fun") input pos) then
            let tok8 = Str.matched_string input in
            if length <= (String.length tok8) then
              (Tok_Fun)::(tok (pos+3))
            else
              (Tok_ID token)::(tok (pos +length))
          else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*in") input pos) then
            let tok9 = Str.matched_string input in
            if length <= (String.length tok9) then
              (Tok_In)::(tok (pos+2))
            else
              (Tok_ID input)::(tok (pos+length))

          else if (Str.string_match re_bool input pos) then
            let tok10 = Str.matched_string input in
            if length <= (String.length tok10) then
              let value = Str.string_match (Str.regexp "[ \n\r\x0c\t]*true") tok10 0 in
              (Tok_Bool value)::(tok (pos+(String.length tok10)))
            else
              (Tok_ID token)::(tok (pos+ length))
          else
            (Tok_ID token)::(tok (pos+length))
          end
          
      else if (Str.string_match re_str input pos) then
        let token = Str.matched_string input in
        (Tok_String token)::(tok (pos+(String.length token)))

      else if (Str.string_match (Str.regexp "[0-9]+[ \n\r\x0c\t]*") input pos) then
        let token = Str.matched_string input in
        (Tok_Int (int_of_string token))::(tok (pos + (String.length token)))

      else if (Str.string_partial_match (Str.regexp "\\(-[0-9]+\\)") input pos) then
        let token = Str.matched_string input in
        let value = String.sub token 1 ((String.length token) -1) in
        (Tok_Int (int_of_string value))::(tok (pos + String.length token))

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*<>[ \n\r\x0c\t]*") input pos) then
        (Tok_NotEqual)::(tok (pos+(String.length (Str.matched_string input))))

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*>=") input pos) then
        (Tok_GreaterEqual)::(tok (pos + (String.length (Str.matched_string input))) )

      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*->|}) input pos) then
        (Tok_Arrow)::(tok (pos+(String.length (Str.matched_string input))))
      
      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*<|}) input pos) then
        
        (Tok_Less)::(tok (pos + (String.length (Str.matched_string input))) )

      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*<=|}) input pos) then
        (Tok_LessEqual)::(tok (pos + (String.length (Str.matched_string input))) )

      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*>|}) input pos) then
        
        (Tok_Greater)::(tok (pos+(String.length (Str.matched_string input))) )

      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*=[ \n\r\x0c\t]*|}) input pos) then
        
        (Tok_Equal)::(tok (pos+(String.length (Str.matched_string input))) )

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*\\+[ \n\r\x0c\t]*") input pos) then
        (Tok_Add)::(tok (pos + (String.length (Str.matched_string input))))

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*-[ \n\r\x0c\t]*") input pos) then
        (Tok_Sub)::(tok (pos+(String.length (Str.matched_string input))) )

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*\\*[ \n\r\x0c\t]*") input pos) then
        (Tok_Mult)::(tok (pos+(String.length (Str.matched_string input))) )

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*\\/[ \n\r\x0c\t]*") input pos) then
        (Tok_Div)::(tok (pos+(String.length (Str.matched_string input))) )

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*\\^") input pos) then
        (Tok_Concat)::(tok (pos+(String.length (Str.matched_string input))))

      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*/|{2}|}) input pos) then
        (Tok_Or)::(tok (pos + (String.length (Str.matched_string input))) )

      else if (Str.string_match (Str.regexp {|[ \n\r\x0c\t]*\&\&|}) input pos) then
        (Tok_And)::(tok (pos+ (String.length (Str.matched_string input))) ) 

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*;;") input pos) then
        (Tok_DoubleSemi)::(tok (pos+(String.length (Str.matched_string input))) )

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*\\(") input pos) then
        (Tok_LParen)::(tok (pos+(String.length (Str.matched_string input))) )

      else if (Str.string_match (Str.regexp "[ \n\r\x0c\t]*\\)") input pos) then
        (Tok_RParen)::(tok (pos+(String.length (Str.matched_string input))) )

      else
        tok (pos+1)

      in tok 0


