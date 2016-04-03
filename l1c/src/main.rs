extern crate regex;

use std::fs::File;
use std::io::Read;
use std::env;
use std::str::Chars;
use regex::Regex;
use std::collections::HashMap;

enum Instruction {
    Mvrr { dst: String, src: String },
    Mvmr { dst: String, src: String, off: String },
    Mvrm { dst: String, src: String, off: String },
    Arop { dst: String, src: String, op: Arithmetic },
    Sfop { dst: String, src: String, op: Shift },
    Comp { dst: String, lhs: String, rhs: String, op: Compare },
    Labl { lab: String },
    Goto { dst: String },
    Cjmp { lhs: String, rhs: String, op: Compare, tru: String, fal: String },
    Call { dst: String, nump: String },
    Tcal { dst: String, nump: String },
    Retn,
    Calp,
    Cala,
    Cale,
}

enum Arithmetic {
    Add,
    Sub,
    Mul,
    And,
}

enum Shift {
    Left,
    Right,
}

enum Compare {
    Less,
    Leeq,
    Eqal,
}

fn main() {
    // argument validation check
    if env::args().count() != 2 {
        println!("Useage: ./l1c filename");
        return;
    }

    // open the source file
    let mut f = match File::open(env::args().nth(1).unwrap()) {
        Ok(f)   => f,
        Err(e)  => { println!("{}", e); return; },
    };

    // get an iterator over the chars of the source file
    // the "chars" method of std::io::Read is unstable
    // use the following wrokaround
    let mut codes = String::new();
    if let Err(e) = f.read_to_string(&mut codes) {
        println!("{}", e);
        return;
    }
    //let mut codes = codes.chars();

    // prepare the regular expressions used by parser
    // w ::= (a|rax|rbx|rbp|r10|r11|r12|r13|r14|r15)
    // x ::= (a|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp)
    // s ::= (a|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|[0-9]+|:[a-zA-Z_][a-zA-Z_0-9]*)
    // t ::= (a|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|[0-9]+)
    // u ::= (a|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|:[a-zA-Z_][a-zA-Z_0-9]*)
    // a ::= (rdi|rsi|rdx|sx|r8|r9)
    // sx::= rcx
    let mut regexs = HashMap::new();
    regexs.insert("MVRR", Regex::new(r"\(\s*(a|rax|rbx|rbp|r10|r11|r12|r13|r14|r15)\s+<-\s+(a|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|[0-9]+|:[a-zA-Z_][a-zA-Z_0-9]*)\s*\)").unwrap());
    regexs.insert("MVMR", Regex::new(r"\(\s*(a|rax|rbx|rbp|r10|r11|r12|r13|r14|r15)\s+<-\s+\(\s*mem\s+(a|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp)\s+[0-9]+\s*\)\s*\)").unwrap());

    //l1_parser(codes);
    l1_instruction_parser(&mut codes, &regexs);
}

fn l1_parser(codes: Chars) {
    for c in codes {
        print!("{}", c);
    }
}

fn l1_instruction_parser(ins: &str, regexs: &HashMap<&str, Regex>) -> Instruction {
    if regexs["MVMR"].is_match(ins) {
        println!("find Mvmr instruction");
    } else if { let re = Regex::new(r"\(\s*\(\s*mem\s+.+\s+.+\s*\)\s+<-\s+.+\s*\)").unwrap();
        re.is_match(ins) } {
        println!("find Mvrm instruction");
    } else if { let re = Regex::new(r"\(\s*.+\s+<-\s+.+\s+(<|<=|=)\s+.+\s*\)").unwrap();
        re.is_match(ins) } {
        println!("find Comp instruction");
    } else if regexs["MVRR"].is_match(ins) {
        println!("find Mvrr instruction");
    }
    return Instruction::Retn;
} 
