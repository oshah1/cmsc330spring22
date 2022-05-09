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

        fn is_compatible_aux(lnk:&Link, v:&i32) -> bool {
            
            
            //match the node
            match lnk {
                None => true,
                Some(n) => {let node = n.read().unwrap();//get the node
                            let version = node.data.version;//get the version
                            if version == *v {//compare the version
                                return is_compatible_aux(&node.rest, &v);
                            } else {
                                return false;
                            }
                    }
            }
              
        }
        let armor = self.armor.head_link.clone();
        is_compatible_aux(&armor,&self.version)
    }

    pub fn repair(&mut self) {
        //first, get the head link
        let head = self.armor.head_link.clone();
        fn repair_aux(curr: &Link) {
            //match the underlying node
            if let Some(n) = curr {
                //extract the node
                let mut node = n.write().unwrap();
                //get the armor
                let mut armor = &mut node.data;
                //match the armor
                match armor.component {
                    
                    Component::LeftThrusters(b, i) if b==true && i < 100 => {armor.component = Component::LeftThrusters(false,100)}, 
                    Component::RightThrusters(b, i) if b==true && i < 100 => {armor.component = Component::RightThrusters(false,100)},
                    Component::LeftRepulsor(b, i) if b==true && i < 100 => {armor.component = Component::LeftRepulsor(false,100)},
                    Component::RightRepulsor(b, i) if b==true && i < 100 => {armor.component = Component::RightRepulsor(false,100)},
                    Component::ChestPiece(b, i) if b==true && i < 100 => {armor.component = Component::ChestPiece(false,100)},
                    Component::Helmet(b) if !b => {armor.component = Component::Helmet(true)},
                         
                    Component::ArcReactor(i) if i < 100 => {armor.component = Component::ArcReactor(100)},
                    Component::Wifi(b) if !b => {armor.component = Component::Wifi(true)},
                    _ => {}
                }
                //get the rest of the node
                let next = &node.rest;
                repair_aux(next);
            }
        }
        repair_aux(&head)
        
    }
}
