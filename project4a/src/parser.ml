open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

  (* Part 3: Parsing mutop *)

let rec parse_mutop toks = failwith "unimplemented"

(* Part 2: Parsing expressions *)

let rec parse_expr toks =
  let (t, exp) = parse_exp toks in
  if t <> Tok_DoubleSemi then
    raise (InvalidInputException "s")
  else
    exp

and parse_exp toks =
  match lookahead toks with
  | Some Tok_Let -> let tok2 = match_token toks Tok_Let in parse_let tok2

  | Some Tok_Fun -> let tok2 = match_token toks Tok_Fun in parse_fun tok2
  | Some Tok_If -> let tok2 = match_token toks Tok_if in parse_if tok2          
  | Some _ ->  parse_or toks

  | None -> raise (InvalidInputException "S")

and parse_let toks =
  match lookahead toks with
  let tok2 = match_token toks Tok_Let in
      let (recurse,tok3) = match lookahead tok2 with
                  | Some Tok_Rec -> (true,match_token tok2 Tok_Rec)
                  | _ -> (false,tok2) in
                  match (lookahead tok3), (lookahead_many tok3 1) with
                  | Some Tok_ID i, Some Tok_Equal -> let tok = match_many tok3 [Tok_ID i;Tok_Equal] in
                                                    let (tok4, exp) = parse_exp tok in
                                                    let (tok5, exp2) = parse_exp (match_token tok4 Tok_In) in
                                                    (tok5, Let (i,recurse,exp1,exp2))
                  | _,_ ->  raise (InvalidInputException "Let")
  

and parse_fun toks =
match lookahead toks with
| Some Tok_ID id -> let tok2 = match_many toks [Tok_ID id;Tok_Arrow] in
                    let (tok3, exp) = parse_exp tok2 in
                    (tok3, Fun (id, exp))
| _ -> raise (InvalidInputException "S")

and parse_if toks =
match lookahead toks with
| Some Tok_If -> let (tok1, exp1) = parse_exp toks in
                let tok2 = match_token tok1 Tok_Then in
                let (tok3, exp2) = parse_exp tok2 in
                let tok4 = match_token tok3 Tok_Else in
                let (tok5, exp3) = parse_exp tok4 in
                (tok5, If (exp1, exp2, exp3))
| _ -> raise (InvalidInputException "S")

and parse_or toks = 

let (tok1, exp1) = parse_and toks in
match lookahead toks with
| Some Tok_Or -> let tok2 = match_token tok1 Tok_Or in
              let (tok3, exp2) = parse_or tok2 in
              (tok3, Binop (Or, exp1, exp2))
| _ -> (tok1, exp1)

and parse_and toks = 
let (tok1, exp1) = parse_equal toks in
match lookahead tok1 with
| Some Tok_And -> let tok2 = match_token tok1 Tok_And in
let (tok3, exp2) = parse_and tok2 in
(tok3, Binop (And, exp1, exp2))
| _ -> (tok1, exp1)

and parse_equal toks =
  let (tok1, exp1) = parse_relate toks in
  match lookahead tok1 with
  | Some Tok_Equal -> let tok2 = match_token tok1 Tok_Equal in
                  let (tok3, exp2) = parse_equal tok2 in
                  (tok3, Binop (Equal, exp1, exp1))
  | Some Tok_NotEqual -> let tok2 = match_token tok1 Tok_NotEqual in
                    let (tok3, exp2) = parse_equal tok2 in
                    (tok3, Binop (NotEqual, exp1, exp1))

and parse_relate toks = 
  let (tok1, exp1) = parse_add tok2 in
  match lookahead tok1 with
  | Some Tok_GreaterEqual-> let tok2 = match_token tok1 Tok_GreaterEqual in
                                let (tok3, exp2) = parse_relate tok2 in
                                (tok3, Binop (GreaterEqual, exp1, exp1))
  | Some Tok_Greater -> let tok2 = match_token tok1 Tok_Greater in
                      let (tok3, exp2) = parse_relate tok2 in
                      (tok3, Binop (Greater, exp1, exp1))
  | Some Tok_LessEqual -> let tok = match_token tok1 Tok_LessEqual in
                      let (tok3, exp2) = parse_relate tok2 in
                      (tok3, Binop (LessEqual, exp1, exp2))
  | Some Tok_Less -> let tok = match_token tok1 Tok_Less in
                let (tok3, exp2) = parse_relate tok2 in
                (tok3, Binop (Less, exp1, exp2))
  | _ -> (tok1, exp1)

and parse_add toks =
let (tok1, exp) = parse_mult toks in
match lookahead tok1 with
| Some Tok_Add -> let tok2 = match_token tok1 Tok_Add in
              let (tok3, exp2) = parse_add tok2 in
              (tok3, Binop (Add, exp, exp2))
| Some Tok_Sub -> let tok2 = match_token tok1 Tok_Sub in
              let (tok3, exp2) = parse_add tok2 in
              (tok3, Binop (Sub, exp, exp2))

| _ -> (tok1, exp)

and parse_mult toks =
let (tok1,exp1) = parse_concat toks in
match lookahead tok1 with
| Some Tok_Mult -> let tok2 = match_token tok1 Tok_Mult in
              let (tok3, exp2) = parse_mult tok2 in
              (tok3, Binop (Mult, exp1, exp2))
| Some Tok_Div -> let tok2 = match_token Tok_Div in
              let (tok3, exp2) = parse_mult tok2 in
              (tok3, Binop (Div, exp1, exp2))

| _ -> (tok1, exp1)

and parse_concat toks =
let (tok1, exp) = parse_unary toks =
match lookahead tok1 with
| Some Tok_Concat -> let tok2 = match_token tok1 Tok_Concat in
                let (tok3, exp2) = parse_concat tok2 in
                (tok3, Binop (Concat, exp, exp2))
| _ -> (tok1,exp)
and parse_unary toks =
(*match token in front with ^*)
match lookahead toks with
| Some t -> let tok2 = match_token Tok_Not t in
            let (tok3, exp) = parse_unary tok2 in
            (tok3, exp)
| None -> parse_primary toks

and parse_functioncall toks = 

let (tok1, exp1) = parse_primary toks in
match lookahead tok1 with
    | (Some Tok_Int _) | (Some Tok_Bool _) | (Some Tok_String _) | (Some Tok_ID _) | (Some Tok_LParen) -> let (tok2, exp2) = parse_primary tok1 in
                                                                                                          (tok2, FunctionCall (exp1, exp2))
    | _ -> (tok1, exp1)


and parse_primary toks =
match lookahead toks with
| Some Tok_Int i -> let tok2 = match_token toks (Tok_Int i) in
            (tok2,Value (Int i))

| Some Tok_Bool b -> let tok2 = match_token toks (Tok_Bool b) in
            (tok2, Value (Bool b))
| Some Tok_String st -> let tok2 = match_token toks (Tok_String st) in
              (tok2, Value (String st))
| Some Tok_ID id -> let tok2 = match_token toks (Tok_ID s) in
              (tok2, ID id)
| Some Tok_LParen -> let tok2 = match_token toks (Tok_LParen) in
              let (tok3, e) = parse_exp tok2 in (*parse the
                                                  expression*)
              let tok4 = match_token tok3 (Tok_RParen) in
              (tok4,e)
| _ -> raise (InvalidInputException "S")


