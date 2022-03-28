open List
open Nfa
open Sets
(*********)
(* Types *)
(*********)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(***********)
(* Utility *)
(***********)

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)

(*creates a set of e_transitions to add to the nfa created by regexp_to_nfa*)
let rec create_e_transitions (fin_states: 'q list) (start: 'q): ('q, 's) transition list =
map (fun x -> (x, None, start)) fin_states

let rec regexp_to_nfa_helper (regexp: regexp_t): (int, char) nfa_t =
  let start = fresh () and fin = fresh () in
match regexp with
  | Empty_String -> {sigma = [];qs = [start]; q0 = start; fs = [start]; delta = []}

  | Char c -> {sigma = [c]; qs = [start;fin];q0 = start;fs = [fin]; delta = [(start, Some c, fin)]}

  | Concat (r1, r2) -> let r1_nfa = regexp_to_nfa_helper r1 and r2_nfa = regexp_to_nfa_helper r2 in
                          (*create new nfa*)
                          {
                            sigma = union r1_nfa.sigma r2_nfa.sigma;(*union both regexp languages*)
                            qs = union r1_nfa.qs r2_nfa.qs;(*and set of states*)
                            q0 = r1_nfa.q0;
                            fs = r2_nfa.fs;
                            delta = union r1_nfa.delta (union r2_nfa.delta (create_e_transitions r1_nfa.fs r2_nfa.q0))
                          }

  | Union (r1,r2) -> let r1_nfa = regexp_to_nfa_helper r1 and r2_nfa = regexp_to_nfa_helper r2 in
                      
                      let finals = create_e_transitions (union r1_nfa.fs r2_nfa.fs) fin in
                      {
                        sigma = union r1_nfa.sigma r2_nfa.sigma;
                        qs = start::(fin::(union r1_nfa.qs r2_nfa.qs));
                        q0 = start;
                        fs = [fin];
                        delta = (start, None, r1_nfa.q0)::((start,None,r2_nfa.q0)::(union r1_nfa.delta (union r2_nfa.delta finals)))
                      }
  | Star reg ->  let reg_nfa = regexp_to_nfa_helper reg in
                  {
                    sigma = reg_nfa.sigma;
                    qs = start::(fin::reg_nfa.qs);
                    q0 = start;
                    fs = [fin];
                    delta = 
                  }
  

let regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t =
  (*create new nfa*)
  
  regexp_to_nfa_helper regexp

(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then [Tok_END]
    else if Str.string_match re_var s pos then
      let token = Str.matched_string s in
      Tok_Char token.[0] :: tok (pos + 1) s
    else if Str.string_match re_epsilon s pos then
      Tok_Epsilon :: tok (pos + 1) s
    else if Str.string_match re_union s pos then Tok_Union :: tok (pos + 1) s
    else if Str.string_match re_star s pos then Tok_Star :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else raise (IllegalExpression ("tokenize: " ^ s))
  in
  tok 0 str

let tok_to_str t =
  match t with
  | Tok_Char v -> Char.escaped v
  | Tok_Epsilon -> "E"
  | Tok_Union -> "|"
  | Tok_Star -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list =
    match tok_list with
    | [] -> raise (IllegalExpression "lookahead")
    | h :: t -> (h, t)
  in
  let rec parse_S l =
    let a1, l1 = parse_A l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Union ->
        let a2, l2 = parse_S n in
        (Union (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_A l =
    let a1, l1 = parse_B l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Char c ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_Epsilon ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_LParen ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_B l =
    let a1, l1 = parse_C l in
    let t, n = lookahead l1 in
    match t with Tok_Star -> (Star a1, n) | _ -> (a1, l1)
  and parse_C l =
    let t, n = lookahead l in
    match t with
    | Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
        let a1, l1 = parse_S n in
        let t2, n2 = lookahead l1 in
        if t2 = Tok_RParen then (a1, n2)
        else raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let rxp, toks = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")


let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str
