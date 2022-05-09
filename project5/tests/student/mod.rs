extern crate stark_suit_repair;

use stark_suit_repair::basics::{
    factorize, gauss, in_range, longest_sequence, mean, rotate, subset, substr, to_decimal,
};
use stark_suit_repair::communicator::{to_command, Command};

// adding linked list tests
use stark_suit_repair::linkedlist::{Armor, Component, List, Suit};

/*
 * Create a new function for each test that you want to run.  Please be sure to add
 * the #[test] attribute to each of your student tests to ensure they are all run, and
 * prefix them all with 'student_' (see example below).
 * Then, run `cargo test student` to run all of the student tests.
 */

#[test]
fn student_example() {
    assert_eq!(true, true);
}
#[test]
fn test_starts() {
    let test1 = "CMSC 330 is the best CS class";
    let slice = &test1[0..];
    let pat = "CMSC 330";
    println!("{}",slice);
    assert!(test1.starts_with("CMSC 330"));
    assert!(test1.starts_with(&slice));
    assert!(slice.starts_with(pat));
}
#[test]
fn test_longseq() {
    
    assert_eq!(Some("aa"), longest_sequence(&"aaba".to_string()));
}

#[test]
fn test_peek_and_pop() {
    let mut new_list = List::new();
    let helm = Armor {
        component: Component::Helmet(true),
        version: 0,
    };
    let wifi = Armor {
        component: Component::Wifi(true),
        version: 0,
    };
    new_list.push(wifi);
    new_list.push(helm);
    assert_eq!(Some(helm),new_list.peek());
    assert_eq!(Some(helm),new_list.pop());
}

#[test]
fn student_misc () {
    assert_eq!(Command::Shield(true), to_command("/shield on/"));
    assert_eq!(Command::Try, to_command("/try calling Miss Potts/"));
    assert_eq!(Command::Invalid, to_command("jarvis!"));
}
