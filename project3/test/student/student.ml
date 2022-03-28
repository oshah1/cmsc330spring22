open P3.Nfa
open P3.Regexp
open TestUtils
open OUnit2

let test_placeholder _ =
  assert_equal true true

let test_e_closure _ =
  let m1 = {sigma = ['a'; 'b']; qs = [0; 1; 2; 3; 4]; q0 = 0; fs = [3];
  delta =
   [(0, Some 'a', 1); (1, None, 2); (2, Some 'b', 2); (2, Some 'a', 3);
    (1, Some 'b', 4); (4, None, 3)]} in
    assert_nfa_closure [1] [1;2];
    assert_nfa_closure [4;1] [4;1;3;2]


let suite =
  "student"
  >::: [ "nfa_new_states" >:: test_placeholder
  ; "test_e_closure" >:: test_e_closure]

let _ = run_test_tt_main suite
