extern crate regex;
use regex::Regex;


#[derive(Debug)]
#[derive(PartialEq)]
pub enum Command
{
    Power(bool,i32),    // [Increase/Decrease] power by [number].
    Missiles(bool,i32), // [Increase/Decrease] missiles by [number].
    Shield(bool),       // Turn [On/Off] the shield.
    Try,                // Try calling pepper.
    Invalid             // [anything else]
}


/**
    Adds functionality to Command enums
    Commands can be converted to strings with the as_str method
    
    Command     |     String format
    ---------------------------------------------------------
    Power       |  /Power (increased|decreased) by [0-9]+%/
    Missiles    |  /Missiles (increased|decreased) by [0-9]+/
    Shield      |  /Shield turned (on|off)/
    Try         |  /Call attempt failed/
    Invalid     |  /Not a command/
**/
impl Command {
    pub fn as_str (&self) -> String {
        let mut output = String::from("");
        if let Command::Power(b,i) = self {
            if *b {
                
                output.push_str("Power increased by ");
                

            } else {
                output.push_str("Power decreased by ");
            }
            let qty = i.to_string();
            output.push_str(&qty);
            output.push_str("%");
        } else if let Command::Missiles(b,i) = self {
            if *b {
                output.push_str("Missiles increased by ");
            } else {
                output.push_str("Missiles decreased by ");
            }
            let qty = i.to_string();
            output.push_str(&qty);
        } else if let Command::Shield(b) = self {
            if *b {
                output.push_str("Shield turned on");
            } else {
                output.push_str("Shield turned off");
            }
        } else if let Command::Try = self {
            output.push_str("Call attempt failed");
        } else {
            output.push_str("Not a command");
        }
        output
    }
}

/**
    Complete this method that converts a string to a command 
    We list the format of the input strings below

    Command     |     String format
    ---------------------------------------------
    Power       |  /power (inc|dec) [0-9]+/
    Missiles    |  /(fire|add) [0-9]+ missiles/
    Shield      |  /shield (on|off)/
    Try         |  /try calling Miss Potts/
    Invalid     |  Anything else
**/
//pwr_cmd.is_match()
pub fn to_command(s: &str) -> Command {
    let pwr_cmd = Regex::new(r"power (inc|dec) ([0-9]+)").unwrap();
        let msl_cmd = Regex::new(r"(fire|add) ([0-9]+) missiles").unwrap();
        let shd_cmd = Regex::new(r"shield (on|off)").unwrap();
        let try_call = Regex::new(r"try calling Miss Potts").unwrap();

        if pwr_cmd.is_match(s) {
            let caps = pwr_cmd.captures(s).unwrap();//get capture groups
            let amt: i32 = (caps.get(2).unwrap().as_str()).parse().unwrap();
            let op = caps.get(1).unwrap().as_str() == "inc";
           
            return Command::Power(op,amt);

        } else if msl_cmd.is_match(s) {
            let caps = msl_cmd.captures(s).unwrap();//get capture groups
            let amt: i32 = (caps.get(2).unwrap().as_str()).parse().unwrap();
            let op = caps.get(1).unwrap().as_str() == "add";//flag for whether we add or fire missiles
            
            return Command::Missiles(op,amt);

        } else if shd_cmd.is_match(s) {
            let caps = shd_cmd.captures(s).unwrap();
            let op = caps.get(1).unwrap().as_str() == "on";//do we turn shields on or off?
            
            return Command::Shield(op);

        } else if try_call.is_match(s) {
            return Command::Try;

        }else{
            return Command::Invalid;
        }
}
