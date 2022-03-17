open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let rec contains lst item =
  match lst with
  | [] -> false
  | h::t -> if h = item then true else contains t item

let rec move_helper (tr: ('q,'s) transition list) (state: 'q) (s: 's option) : 'q list =
  match s with
    | None -> (match tr with
                | [] -> []
                | (a, None, c)::t -> if a=state then c::(move_helper t state s) else move_helper t state s
                | (_,_,_)::t -> move_helper t state s)
    | Some ch -> (match tr with
                  | [] -> []
                  | (a, Some b, c)::t -> if a = state && b = ch then c::(move_helper t state s) else move_helper t state s
                  | (_,_,_)::t -> move_helper t state s)
  
let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  let transitions = nfa.delta in
  (*union two lists together*)
  let union lst1 lst2 =
    (*if lst1 already contains the head, don't cons it to lst1*)
    fold_right (fun x a -> if contains lst1 x then a else x::a) lst1 lst2 in
  (*move_helper returns a list of final states reachable with a
  transition from a given state and character
  fold right on the list move_helper returns*)
  fold_right (fun h acc -> union acc (move_helper transitions h s) ) qs []
  
  

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  failwith "unimplemented"

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  failwith "unimplemented"

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  failwith "unimplemented"

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  failwith "unimplemented"

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  failwith "unimplemented"

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "unimplemented"

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  failwith "unimplemented"
