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

(*union two lists together*)
let rec union lst1 lst2 =
  (*if lst1 already contains the head, don't cons it to lst1*)
  match lst1 with
  | [] -> lst2
  | h::t -> if contains lst2 h then union t lst2 else union t (h::lst2)

let rec move_helper (tr: ('q,'s) transition list) (state: 'q) (s: 's option) : 'q list =
  match s with
    | None -> (match tr with
                | [] -> []
                | (a, None, c)::t -> if a=state then c::(move_helper t state s) else move_helper t state s
                | (_,_,_)::t -> move_helper t state s)
    | Some ch -> (match tr with
                  | [] -> []
                  | (a, Some b, c)::t -> if a = state && ch = b then c::(move_helper t state s) else move_helper t state s
                  | (_,_,_)::t -> move_helper t state s)
                  




let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  let transitions = nfa.delta in
  
  
  (*move_helper returns a list of final states reachable with a
  transition from a given state on a character
  fold right on the list move_helper returns*)
  fold_right (fun h a -> union (move_helper transitions h s) a) qs []
  
  

(*this function takes an nfa, a list of states, returns a list of states that can be reached w/ 0 or more
epsilon transitions*)
let rec e_closure_helper (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  
  match qs with
    | [] -> []
    | h::t -> e_closure_helper nfa (union qs (move nfa qs None))

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =

  e_closure_helper nfa qs
  
(*this function calls move repeatedly with a nfa and string, returning a list of states 
the nfa could be in after the string is empty*)
let rec accept_helper (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: char list) : 'q list = 

(*perform move on list of states and first character of s*)
match s with
[] -> e_closure nfa qs
| h::t -> let transition_states = union (e_closure nfa qs) (move nfa qs (Some h)) in
              accept_helper nfa transition_states t 

(*recurse thru list of states returned by nfa_helper, checking if any of them are one of
    the nfa's final states*)
let rec end_in_final (states: 'q list) (final_states: 'q list): bool=
match states with
| [] -> false
| h::t -> if contains final_states h then true else end_in_final t final_states

let accept (nfa: ('q, 's) nfa_t) (s: string) : bool =
  let string_arr = explode s in
  let nfa_final = nfa.fs in
  let nfa_start = nfa.qs in
  
  let end_states = accept_helper nfa nfa_start string_arr in 
  
  
  end_in_final end_states nfa_final
  

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let rec concat lst1 lst2 =
  match lst1 with
  [] -> lst2
  | h::t -> concat t (h::lst2)


(*returns a list of all transitions possible from a given state with all letters in an nfa's alphabet*)
let rec get_character_transitions (nfa: ('q,'s) nfa_t) (sigma: 's list) (state: 'q) : 'q list list =
let transitions c = move nfa [state] (Some c) in
let epsilon c= e_closure nfa (transitions c) in
let all_transitions c = union (transitions c) (epsilon c) in

match sigma with
| [] -> []
| h::t -> (all_transitions h)::(get_character_transitions nfa t state)

  (*gets all the character transitions possible on a single state (incl epsilon transitions) and concatenates them
  to an accumulator*)
let rec new_states_helper (nfa: ('q,'s) nfa_t) (states: 'q list) (sigma: 's list) (acc: 'q list list): 'q list list=
  match states with
  | [] -> acc
  | h::t -> union acc (get_character_transitions nfa sigma h)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  
  (*get the alphabet we will be using for getting the sets of states*)
  new_states_helper nfa qs nfa.sigma []



let rec new_trans_helper (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('a * 'b) list =
  let sigma = nfa.sigma in
match qs with
| [] -> []
| h::t -> let ch_transitions = fold_right (fun x a-> (Some x, move nfa [h] (Some x))::a) sigma [] in
      if contains nfa.qs h then
        (*now, add any e_transitions*)
        let add_epsilon = map (fun (x, y) -> (x, union y (e_closure nfa y))) ch_transitions in
        concat add_epsilon (new_trans_helper nfa t)
      else
        concat ch_transitions (new_trans_helper nfa t)



let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  (*must return: list of q list, s transitions
  How to construct: ([transition list], char, end state)
  1: Send list of states to a recursive function
  - Function will take list of states, a list of characters, and and accumulator and
  return a list of q list s transitions
  2: For each state, call move on it and every character in nfa alphabet, store results in a list
  3: List is a ('s, 'q) tuple list, which will be concatenated to the accumulator
  4: also get any e_closures on the state, and also concatenate that list to the accumulator
  5: After getting all the transitions on actual characters, do the epsilon transitions separately*)
  let transitions = map (fun (a, b) -> (qs,a, b)) (new_trans_helper nfa qs) in
  (*Now, get all the e_transitions*)
  fold_right (fun x acc-> if contains nfa.qs x then (qs, None, [x])::acc else acc) qs transitions

let rec new_finals_helper (nfa: ('q,'s) nfa_t) (qs: 'q list) : bool =
  let final = nfa.fs in
  match qs with
  | [] -> false
  | h::t -> if contains final h then true else new_finals_helper nfa t

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  (*get list of final states from nfa
  recurse thru qs, if nfa. fs contains a state, return qs, else return empty list*)
    if new_finals_helper nfa qs then 
      [qs] 
    else 
      []

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "unimplemented"

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  failwith "unimplemented"
