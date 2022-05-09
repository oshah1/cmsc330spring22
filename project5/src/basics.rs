/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    if n < 0 {
        return -1
    } else if n == 0 {
        return 1
    } else {
        return (n * (n + 1))/2
    }
}

/**
    Returns the number of elements in the list that 
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut count = 0;
    for i in ls.iter() {
        if (*i >= s) && (*i <= e) {
            count = count + 1;
        }
    }
    let result = count;
    result
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    let mut result = true;
    for i in target.iter() {
        let mut has_elem = false;
        /*check if set contains an element in target*/
        //if so, do nothing
        //else, set result to false
        for j in set.iter() {
            if *i == *j {
                has_elem = true;
            }

        }
        if !has_elem {
            result = false
        }
    }
    result
}

/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    if ls.len() == 0 
    {
        None
    } else {
        let sum = ls.iter().fold(0.0, |acc, i| acc + i);
       let length = ls.len() as f64;
       Some (sum/length)
    }

}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
    
    let mut counter = 0;
    let mut result = 0;
    for i in ls.iter().rev() {
        let xpow = 2_i32.pow(counter);
        result = result + (*i * xpow);
        counter = counter + 1;
    }
    result
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
//assume every number passed in is at least 2

pub fn factorize(n: u32) -> Vec<u32> {
    let mut result:Vec<u32> = Vec::new();
    /*Find the first number that divides 36, then factorize the divisor*/
    let mut num = n;
    //figure out how many 2's divide n
    while num % 2 ==0 {
        result.push(2);
        num = num/2;
    }
        //get the square root of n
        
        let mut i = 3;
        while (i as f64) <= (num as f64).sqrt() {
            while num%i == 0 {
                result.push(i);
                num = num/i;
            }
            i = i + 2;
        }

        if num > 2 {
            result.push(num);
        }
        result
    }
    


/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    let mut result = Vec::new();
    
    
    for i in lst.iter() {
        result.push(*i);
    }
    //remove the first element
    let first = result.remove(0);
    //push it onto the end of result
    result.push(first);
    result
}
    
/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    
    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    
    let s_len = s.len();
    let target_len = target.len();
    for i in 0..(s_len-target_len+1) {
        let sl = &s[i..];//extract a slice from i onwards
        println!("{}",&target);
        println!("{}",&sl);
        if sl.starts_with(target) {return true;}
        
    }
    false
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    let mut result = None;//stores result
    let mut prev_char = None;//stores previous character
    let mut max_seq_len = 0;//stores length of longest sequence
    let mut max_seq_start = 0;
    
    let mut curr_seq_len = 0;//current sequence length
    let mut curr_seq_start = 0;
    for (pos, ch )in s.char_indices() {
        
        if let Some(c) = prev_char {
            //if c matches the current char,
            //the sequence continues
            //otherwise, we start a new sequence
            if c != ch {
                //we are starting a new sequence
                //reset curr_seq_len
                
                curr_seq_len = 0;
                //set current sequence start to the current
                //position
                curr_seq_start = pos;

            }
        }//else do nothing
        //println!("c: {} pos: {}",&ch,&pos);
        prev_char = Some(ch);
        curr_seq_len = curr_seq_len + 1;
        if curr_seq_len > max_seq_len {
            max_seq_len = curr_seq_len;
            max_seq_start = curr_seq_start;
        }
        
    }
    
    if max_seq_len > 0 {
        result = Some (&s[max_seq_start..max_seq_start + max_seq_len]);
    }
    result
}
