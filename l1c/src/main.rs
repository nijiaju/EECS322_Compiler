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
    Prit,
    Aloc,
    Arer,
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

struct Function {
    labl: String,
    narg: String,
    nspl: String,
    inst: Vec<String>,
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
    let mut codes = codes.chars();

    // prepare the regular expressions used by parser
    // a ::= (rdi|rsi|rdx|sx|r8|r9)
    // w ::= (rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15)
    // x ::= (rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp)
    // s ::= (rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|-?[0-9]+|:[a-zA-Z_][a-zA-Z_0-9]*)
    // t ::= (rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|-?[0-9]+)
    // u ::= (rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|:[a-zA-Z_][a-zA-Z_0-9]*)
    // sx::= rcx
    let mut regexs = HashMap::new();
    regexs.insert("MVRR",
    Regex::new(r"^\(\s*(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15)\s+<-\s+(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|-?[0-9]+|:[a-zA-Z_][a-zA-Z_0-9]*)\s*\)$").unwrap());
    regexs.insert("MVMR",
    Regex::new(r"^\(\s*(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15)\s+<-\s+\(\s*mem\s+(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp)\s+-?[0-9]+\s*\)\s*\)$").unwrap());
    regexs.insert("MVRM",
    Regex::new(r"^\(\s*\(\s*mem\s+(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp)\s+-?[0-9]+\s*\)\s+<-\s+(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|-?[0-9]+|:[a-zA-Z_][a-zA-Z_0-9]*)\s*\)$").unwrap());
    regexs.insert("AROP", 
    Regex::new(r"^\(\s*(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15)\s+(\+=|-=|\*=|&=)\s+(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|-?[0-9]+)\s*\)$").unwrap());
    regexs.insert("SFOP", 
    Regex::new(r"^\(\s*(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15)\s+(<<=|>>=)\s+(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|-?[0-9]+|:[a-zA-Z_][a-zA-Z_0-9]*)\s*\)$").unwrap());
    regexs.insert("COMP",
    Regex::new(r"^\(\s*(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15)\s+<-\s+(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|-?[0-9]+)\s+(<|<=|=)\s+(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|-?[0-9]+)\s*\)$").unwrap());
    regexs.insert("LABL",
    Regex::new(r"^\s*:[a-zA-Z_][a-zA-Z_0-9]*\s*$").unwrap());
    regexs.insert("GOTO",
    Regex::new(r"^\(\s*goto\s+:[a-zA-Z_][a-zA-Z_0-9]*\s*\)$").unwrap());
    regexs.insert("CJMP",
    Regex::new(r"^\(\s*cjump\s+(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|-?[0-9]+)\s+(<|<=|=)\s+(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|-?[0-9]+)\s+:[a-zA-Z_][a-zA-Z_0-9]*\s+:[a-zA-Z_][a-zA-Z_0-9]*\s*\)$").unwrap());
    regexs.insert("CALL",
    Regex::new(r"^\(\s*call\s+(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|:[a-zA-Z_][a-zA-Z_0-9]*)\s+[0-9]+\s*\)$").unwrap());
    regexs.insert("TCAL",
    Regex::new(r"^\(\s*tail-call\s+(rdi|rsi|rdx|sx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp|:[a-zA-Z_][a-zA-Z_0-9]*)\s+[0-6]\s*\)$").unwrap());
    regexs.insert("RETN", Regex::new(r"^\(\s*return\s*\)$").unwrap());
    regexs.insert("PRIT", Regex::new(r"^\(\s*call\s+print\s+1\s*\)$").unwrap());
    regexs.insert("ALOC", Regex::new(r"^\(\s*call\s+allocate\s+2\s*\)$").unwrap());
    regexs.insert("ARER", Regex::new(r"^\(\s*call\s+array-error\s+2\s*\)$").unwrap());

    //l1_parser(codes);
    if let Err(e) = l1_function_parser(codes) {
        println!("{}", e);
    }
}

fn l1_parser(codes: Chars) {
}

fn l1_function_parser(mut codes: Chars) -> Result<(), String> {
    // skip all whitespaces until find a '('
    while let Some(c) = codes.next() {
        if c.is_whitespace() {
            continue;
        } else if c != '(' {
            return Err("[Syntax Error] Program Start With Non-Parenthesis Character".to_string());
        } else {
            break;
        }
    }

    // extract the label
    let mut label = String::new();
    while let Some(c) = codes.next() {
        if c.is_whitespace() {
            continue;
        } else if c != ':' {
            return Err("[Syntax Error] Program Label Is Expected".to_string());
        } else {
            //label.push(c);
            break;
        }
    }
    while let Some(c) = codes.next() {
        if !c.is_whitespace() {
            label.push(c);
        } else {
            break;
        }
    }
    println!("[l1_function_parser] Label: {}", label);

    // extract the number of arguments
    let mut narg = String::new();
    while let Some(c) = codes.next() {
        if c.is_whitespace() {
            continue;
        } else if c.is_numeric() {
            narg.push(c);
            break;
        } else {
            return Err("[Syntax Error] Number of Arguments Is Expected".to_string());
        }
    }
    while let Some(c) = codes.next() {
        if c.is_whitespace() {
            break;
        } else if c.is_numeric() {
            narg.push(c);
        } else {
            return Err("[Syntax Error] Number of Arguments Is Expected".to_string());
        }
    }
    println!("[l1_function_parser] Number of Arguments: {}", narg);

    // extract the space needed
    let mut nspl = String::new();
    while let Some(c) = codes.next() {
        if c.is_whitespace() {
            continue;
        } else if c.is_numeric() {
            nspl.push(c);
            break;
        } else {
            return Err("[Syntax Error] Number of Space Is Expected".to_string());
        }
    }
    while let Some(c) = codes.next() {
        if c.is_whitespace() {
            break;
        } else if c.is_numeric() {
            nspl.push(c);
        } else {
            return Err("[Syntax Error] Number of Space Is Expected".to_string());
        }
    }
    println!("[l1_function_parser] Space of Spill: {}", nspl);

    // extract instructions

    return Ok(());
}

fn l1_instruction_parser(ins: &str, regexs: &HashMap<&str, Regex>) -> Instruction {
    if regexs["MVMR"].is_match(ins) {
        println!("find Mvmr instruction: {}", ins);
    } else if regexs["MVRM"].is_match(ins) {
        println!("find Mvrm instruction: {}", ins);
    } else if regexs["MVRR"].is_match(ins) {
        println!("find Mvrr instruction: {}", ins);
    } else if regexs["AROP"].is_match(ins) {
        println!("find Arop instruction");
    } else if regexs["SFOP"].is_match(ins) {
        println!("find Sfop instruction");
    } else if regexs["COMP"].is_match(ins) {
        println!("find Comp instruction");
    } else if regexs["LABL"].is_match(ins) {
        println!("find Label");
    } else if regexs["GOTO"].is_match(ins) {
        println!("find Goto instruction");
    } else if regexs["CJMP"].is_match(ins) {
        println!("find Cjmp instruction");
    } else if regexs["CALL"].is_match(ins) {
        println!("find Call instruction");
    } else if regexs["TCAL"].is_match(ins) {
        println!("find Tcal instruction");
    } else if regexs["RETN"].is_match(ins) {
        println!("find Retn instruction");
    } else if regexs["PRIT"].is_match(ins) {
        println!("find Prit instruction");
    } else if regexs["ALOC"].is_match(ins) {
        println!("find Aloc instruction");
    } else if regexs["ARER"].is_match(ins) {
        println!("find Arer instruction: {}", ins);
    } else {
        println!("[Syntax Error] Instruction Format Error: {}", ins);
    }
    return Instruction::Retn;
} 
