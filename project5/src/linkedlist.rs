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
type Link = Arc<RwLock<Option <Node>>>;//Must store a node, but may be empty too

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
        return List {head_link: Arc::new(RwLock::new(None)), size: 8}
        
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn peek(&self) -> Option<Armor> {
        //get a Node type
        let head = self.head_link.deref();
        let node = *(head.borrow());
        match node {
            None => None,
            Some(n) => {let a = n.data.clone();//clone data in node
                            return Some(a);}
        }
        //dereference head for pattern matching
        
    }

    pub fn push(&mut self, component: Armor) {
        //first, get the head link
        let lnk = self.head_link;

        //Create a new now that has the component as its data and the old head as its link
        let node = Node {data : component, rest: lnk};
        self.head_link = Arc::new(RwLock::new(Some (node)));
    }

    pub fn pop(&mut self) -> Option<Armor> {
        //first, get the head link
        let head = self.head_link;
        //get the underlying node
        let node = *(head.deref()).borrow_mut();
        //pattern match
        match node {
            None => None,
            Some(n) => {let output = Some (n.data);
                        //advance head link
                        self.head_link = Arc::clone(node.rest);
                        return output;}
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
