open OUnit2
open P2b.Data
open P2b.Funs
open P2b.Higher
open TestUtils

let test_sanity _ = 
  assert_equal 1 1

let test_contains_elem _ =
    let a = [1;2;3;4;5] in
    let empty = [] in
	assert_equal ~printer:string_of_bool false (contains_elem empty "1");
	assert_equal ~printer:string_of_bool true (contains_elem a 2)

let test_ap _=
  let funcs = [((+) 2); (fun x-> x*x)] in
  let a = [1;5;7;9;3;6] in
  assert_equal ~printer:string_of_int_list [3;7;9;11;5;8;1;25;49;81;9;36] @@ ap funcs a

let test_scope =
  let a = push_scope empty_table in
  let a = add_var "a" 10 a in
  let a = add_var "b" 40 a in
  let b = push_scope a in
  let b = add_var "a" 20 b in
  let b = add_var "c" 30 b in
  assert_equal 20 (lookup "a" b) ~msg:"student_scopes (2)"

let suite =
  "student" >::: [
    "sanity" >:: test_sanity;
    "contains_elem" >:: test_contains_elem;
    "test_ap" >:: test_ap
  ]

let _ = run_test_tt_main suite
