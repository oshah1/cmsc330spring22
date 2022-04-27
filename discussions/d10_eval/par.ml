open Lexer

(* Types *)
type expr =
| Int of int
| Plus of expr * expr
| Mult of expr * expr

(* Provided helper function - takes a token list and an exprected token.
 * Handles error cases and returns the tail of the list *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (Failure(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (Failure(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

let lookahead toks = match toks with
	 h::t -> h
	| _ -> raise (Failure("Empty input to lookahead"))

(* Parses a token list. *)
let rec parser (toks : token list) : expr =
  let (toks, exp) = parse_S toks in
  if toks <> [Tok_EOF] then
    raise (Failure "did not reach EOF")
  else
    exp
  

(* Parses the S rule. *)
and parse_S (toks : token list) : (token list * expr) =
  let (toks1, exp) = parse_M toks in
  match lookahead toks1 with
  | Tok_Plus -> let toks2 = match_token toks1 Tok_Plus in
                  let (toks3, exp2) = parse_S tok2 in
                  (toks3, Plus (exp, exp2))
  | _ -> (toks1, exp)
  

(* Parses the M rule. *)
and parse_M (toks : token list) : (token list * expr) =
  let (toks1, exp) = parse_N toks in
  match lookahead toks1 with
  | Tok_Mult -> let toks2 = match_token toks1 Tok_Mult in
                let (toks3, exp2) = parse_M toks2 in
                (toks3, Muls(exp, exp2))

  | _ -> (toks1,exp)
            
(* Parses the N rule. *)
and parse_N (toks : token list) : (token list * expr) =
  mach lookahead toks with
  | Tok_int i -> let toks2 = match_token toks (Tok_Int i) with
                  (toks, Int i)
  | Tok_LParen -> let toks2 = match_token toks (Tok_LParen) in
                  let (toks3, exp) = parse_S toks2 in
                  let toks4 = match_token toks3 (Tok_RParen) in
                  (toks4, exp)
  | _ -> failwith "parse failed"