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
let rec e_closure_helper (nfa: ('q,'s) nfa_t) (qs: 'q list) (output: 'q list): 'q list =
  if eq output qs then (*no new states*)
    output
  else
    (*let R = output
    Let output = R union (move nfa R None)*)
    e_closure_helper nfa output (union output (move nfa output None))
  

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =

  e_closure_helper nfa qs (union qs (move nfa qs None))
  
(*this function takes in an nfa a list of strings and the set of states
the nfa could currently be in*)
let rec accept_helper (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: char list) : 'q list = 

(*perform move on list of states and first character of s*)
match s with
[] -> e_closure nfa qs
| h::t -> (*perform an e_closure on qs, then move on the states returned*)
       accept_helper nfa (e_closure nfa (move nfa qs (Some h))) t

let accept (nfa: ('q, 's) nfa_t) (s: string) : bool =
  let string_arr = explode s in
  
  let end_states = accept_helper nfa (e_closure nfa [nfa.q0]) string_arr in 
  (*check if there is no overlap b/w nfa.fs and end_states. if there isn't, don't accept the string*)
 match intersection end_states nfa.fs with
  | [] -> false
  | _ -> true
(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let rec concat lst1 lst2 =
  match lst1 with
  [] -> lst2
  | h::t -> concat t (h::lst2)
  

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  
  
  (*get the alphabet we will be using for getting the sets of states*)
  List.fold_right (fun x a -> insert (e_closure nfa (move nfa qs (Some x))) a) nfa.sigma []


(*takes an nfa, a list of states, and a character, performes a move on the given states, stores the result,
performs and e_closure on the result, and returns it*)
let new_trans_helper (nfa: ('q,'s) nfa_t) (letter: 's) (qs: 'q list) : 'q list =
  let possible_transitions =
    move nfa qs (Some letter) in
  insert_all possible_transitions (e_closure nfa possible_transitions)


let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  
  (*fold thru all letters in the nfa's alphabet, calling new_trans_helper on them*)
  let alphabet = nfa.sigma in
  map (fun c -> (qs, Some c, (new_trans_helper nfa c qs))) alphabet
  

let rec new_finals_helper (nfa: ('q,'s) nfa_t) (qs: 'q list) : bool =
  let final = nfa.fs in
  match qs with
  | [] -> false
  | h::t -> if elem h final then true else new_finals_helper nfa t

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  (*get list of final states from nfa
  recurse thru qs, if nfa. fs contains a state, return qs, else return empty list*)
    if new_finals_helper nfa qs then 
      [qs] 
    else 
      []

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
      match work with
      [] -> {sigma = dfa.sigma;
      qs = dfa.qs; q0 = dfa.q0;
       fs = List.fold_right (fun x a -> if (eq (intersection nfa.fs x)) [] then a else insert x a) dfa.qs []; 
       delta = dfa.delta}
      | h::t -> let moves = minus (new_states nfa h) [[]] in (*calculate all states reachable from h
      but remove the empty list*)
              let transitions = new_trans nfa h in (*may have duplicates*)
              let new_work = insert_all moves t in (*add unvisited states to work*)
              let new_dfa = {
                sigma = dfa.sigma;
                qs = insert_all moves dfa.qs;
                q0 = dfa.q0;
                fs = [];(*if r has a state(s) that exist(s) in nfa.fs, add them to Fd*)
                delta = union dfa.delta transitions
              } in nfa_to_dfa_step nfa new_dfa (minus new_work dfa.qs)


let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  (*get e_closure on nfa star states, add it to worklist*)
  let start = e_closure nfa [nfa.q0]
   in nfa_to_dfa_step nfa {
    sigma = nfa.sigma;
    qs = [start];
    q0 = start;
    fs = [];
    delta = []
  } [start] 
