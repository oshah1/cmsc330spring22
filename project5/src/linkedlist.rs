use std::{
    borrow::BorrowMut,
    ops::{Deref, DerefMut},
    sync::{Arc, RwLock},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Component {
    Helmet(bool),              //is damaged?
    LeftThrusters(bool, i32),  //is damaged? How much power left?
    RightThrusters(bool, i32), //is damaged? How much power left?
    LeftRepulsor(bool, i32),   //is damaged? How much power left?
    RightRepulsor(bool, i32),  //is damaged? How much power left?
    ChestPiece(bool, i32),     //is damaged? How much power left?
    Missiles(i32),             //how many missiles left?
    ArcReactor(i32),           // How much power left?
    Wifi(bool),                // connected to wifi?
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Armor {
    pub component: Component,
    pub version: i32,
}

// Part 2

// Students should fill in the Link type themselves. The Node and List types are given as is.
type Link = Option<Arc<RwLock<Node>>>;//Must store a node, but may be empty too

struct Node {
    data: Armor,
    rest: Link,
}

#[derive(Clone)]
pub struct List {
    head_link: Link,
    size: usize,
}

impl List {
    pub fn new() -> Self {
        return List {head_link: None, size: 0}
        
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn peek(&self) -> Option<Armor> {
        //borrow head
        let head = &self.head_link;
        match head {
            None => return None,
            Some(link) => {let node = (link.deref()).read().unwrap();//borrow it
                            /*borrow_mut returns Node.
                            get data field of node, borrow it*/
                        let data = node.data;
                        
                    return Some(data);}
        }
        //dereference head for pattern matching
        
    }

    pub fn push(&mut self, component: Armor) {
        //first, get the head link
        
         match &self.head_link {
             Some(prev_head) => {//reference prev_head
                                    let temp = prev_head.clone();
                                    self.head_link = Some(Arc::new(RwLock::new(Node {data: component, rest: Some(temp)})))
                                },
             None => {self.head_link= Some(Arc::new(RwLock::new(Node {data: component, rest: None})));
                                    self.size = self.size + 1;}
         }
        
    }

    pub fn pop(&mut self) -> Option<Armor> {
        //pattern match
        match &self.head_link.clone() {
            None => None,
            Some(head) => {
                            let output = (head).read().unwrap();//extract node
                            //reassign head link to output.rest
                             self.head_link = output.rest.clone();
                             self.size = self.size - 1;
                             return Some(output.data);
                            }
        }
        
        
        
    }
}

// Part 3

#[derive(Clone)]
pub struct Suit {
    pub armor: List,
    pub version: i32,
}

impl Suit {
    
    pub fn is_compatible(&self) -> bool {

        fn is_compatible_aux(lst:&mut List, v:&i32) -> bool {
            
            //pop armor off of lst
            let curr = lst.pop();
            //match curr
            match curr {
                None => true,
                Some (n) => {if n.version == *v {
                        is_compatible_aux(lst,&v)
                    } else {
                        false
                    }
                }

            }
            
        }
        let mut armor = self.armor.clone();
        is_compatible_aux(&mut armor,&self.version)
    }

    pub fn repair(&mut self) {
        unimplemented!()
    }
}
