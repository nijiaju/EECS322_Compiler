extern crate regex;

use std::fs::File;
use std::io::Read;
use std::env;
use std::str::Chars;
use regex::Regex;
use std::collections::HashMap;

#[derive(Debug)]
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
    Call { dst: String, arg: String },
    Tcal { dst: String, arg: String },
    Retn,
    Prit,
    Aloc,
    Arer,
}

#[derive(Debug)]
enum Arithmetic {
    Add,
    Sub,
    Mul,
    And,
}

#[derive(Debug)]
enum Shift {
    Left,
    Right,
}

#[derive(Debug)]
enum Compare {
    Less,
    Leeq,
    Eqal,
}

#[derive(Debug)]
struct Program {
    labl: String,
    func: Vec<Function>,
}

impl Program {
    fn new() -> Program {
        Program {
            labl: String::new(),
            func: Vec::new()
        }
    }
}

#[derive(Debug)]
enum ProgExpect {
    Label,
    Func,
}

impl ProgExpect {
    fn next(&mut self) {
        match *self {
            ProgExpect::Label => *self = ProgExpect::Func,
            ProgExpect::Func  => *self = ProgExpect::Func,
        }
    }
}

#[derive(Debug)]
struct Function {
    labl: String,
    narg: String,
    nspl: String,
    inst: Vec<Instruction>,
}

impl Function {
    fn new() -> Function {
        Function {
            labl: String::new(),
            narg: String::new(),
            nspl: String::new(),
            inst: Vec::new()
        }
    }
}

#[derive(Debug)]
enum FuncExpect {
    Label,
    NumArg,
    NumSpl,
    Ins,
}

impl FuncExpect {
    fn next(&mut self) {
        match *self {
            FuncExpect::Label  => *self = FuncExpect::NumArg,
            FuncExpect::NumArg => *self = FuncExpect::NumSpl,
            FuncExpect::NumSpl => *self = FuncExpect::Ins,
            FuncExpect::Ins    => *self = FuncExpect::Ins,
        }
    }
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

    let parse_result = l1_parser(&mut codes, &regexs);
    match parse_result {
        Ok(p)  => println!("{:?}", p),
        Err(e) => println!("{}", e),
    }
}

fn l1_parser(codes: &mut Chars, regexs: &HashMap<&str, Regex>) -> Result<Program, String> {
    while let Some(c) = codes.next() {
        if c.is_whitespace() {
            continue;
        } else if c == '(' {
            break;
        } else {
            return Err("[Syntax Error] Cannot find a Program".to_string());
        }
    }
    return l1_program_parser(codes, regexs);
}

fn l1_program_parser(codes: &mut Chars, regexs: &HashMap<&str, Regex>) -> Result<Program, String> {
    let mut status = ProgExpect::Label;
    let mut p = Program::new();
    'prog_outer: loop {
        match status {
            ProgExpect::Label => {
                let mut label = String::new();
                while let Some(c) = codes.next() {
                    if c.is_whitespace() {
                        continue;
                    } else if c == ':' {
                        break;
                    } else {
                        return Err("[Syntax Error] Program: Label Is Expected".to_string());
                    }
                }
                while let Some(c) = codes.next() {
                    if c.is_whitespace() {
                        break;
                    } else {
                        label.push(c);
                    }
                }
                println!("[l1_program_parser] Label: {}", label);
                p.labl = label;
            }
            ProgExpect::Func => {
                while let Some(c) = codes.next() {
                    if c.is_whitespace() {
                        continue;
                    } else if c == '(' {
                        match l1_function_parser(codes, regexs) {
                            Ok(f)  => p.func.push(f),
                            Err(e) => return Err(e),
                        }
                    } else if c == ')' {
                        break 'prog_outer;
                    } else {
                        return Err("[Syntax Error] Program: Functions Are Expected".to_string());
                    }
                }
            }
        }
        status.next();
    }
    return Ok(p);
}

fn l1_function_parser(codes: &mut Chars, regexs: &HashMap<&str, Regex>) -> Result<Function, String> {
    let mut status = FuncExpect::Label;
    let mut f = Function::new();
    'func_outer: loop {
        match status {
            FuncExpect::Label => {
                let mut label = String::new();
                while let Some(c) = codes.next() {
                    if c.is_whitespace() {
                        continue;
                    } else if c == ':' {
                        break;
                    } else {
                        return Err("[Syntax Error] Function: Label Is Expected".to_string());
                    }
                }
                while let Some(c) = codes.next() {
                    if c.is_whitespace() {
                        break;
                    } else {
                        label.push(c);
                    }
                }
                println!("[l1_function_parser] Label: {}", label);
                f.labl = label;
            }
            FuncExpect::NumArg | FuncExpect::NumSpl => {
                let mut number = String::new();
                while let Some(c) = codes.next() {
                    if c.is_whitespace() {
                        continue;
                    } else if c.is_numeric() {
                        number.push(c);
                        break;
                    } else {
                        return Err("[Syntax Error] Function: Numbers Are Expected".to_string());
                    }
                }
                while let Some(c) = codes.next() {
                    if c.is_whitespace() {
                        break;
                    } else if c.is_numeric() {
                        number.push(c);
                    } else {
                        return Err("[Syntax Error] Function: Numbers Are Expected".to_string());
                    }
                }
                println!("[l1_function_parser] Number: {}", number);
                match status {
                    FuncExpect::NumArg => f.narg = number,
                    FuncExpect::NumSpl => f.nspl = number,
                    _ => {},
                }
            }
            FuncExpect::Ins => {
                let mut ins = String::new();
                let mut i = 0;
                while let Some(c) = codes.next() {
                    if c.is_whitespace() {
                        continue;
                    } else if c == '(' {
                        ins.push(c);
                        ins.push(' ');
                        i = 1;
                        break;
                    } else if c == ')' {
                        break 'func_outer;
                    } else if c == ':' {
                        ins.push(c);
                        break;
                    } else {
                        return Err("[Syntax Error] Function: Instructions Are Expected".to_string());
                    }
                }
                while let Some(c) = codes.next() {
                    if i == 0 {
                        if c.is_whitespace() {
                            break;
                        } else {
                            ins.push(c);
                        }
                    } else {
                        //ins.push(c);
                        if c == '(' {
                            ins.push(c);
                            ins.push(' ');
                            i += 1;
                            if i == 3 {
                                return Err("[Syntax Error] Instruction: Nested Instructions Are Not Allowed".to_string());
                            }
                        } else if c == ')' && i == 1 {
                            ins.push(' ');
                            ins.push(c);
                            break;
                        } else if c == ')' {
                            ins.push(' ');
                            ins.push(c);
                            i -= 1;
                        } else {
                            ins.push(c)
                        }
                    }
                }
                //println!("[l1_function_parser] Instruction: {}", ins);
                match l1_instruction_parser(&ins, regexs) {
                    Ok(i)  => {
                        f.inst.push(i);
                    },
                    Err(e) => return Err(e),
                }
            }
        }
        status.next();
    }

    return Ok(f);
}

fn l1_instruction_parser(ins: &str, regexs: &HashMap<&str, Regex>) -> Result<Instruction, String> {
    let result: Vec<&str> = ins.split_whitespace().collect();
    if regexs["MVMR"].is_match(ins) {
        println!("find Mvmr instruction: {}", ins);
        return Ok(Instruction::Mvmr{ dst: result[1].to_string(),
                                     src: result[5].to_string(),
                                     off: result[6].to_string() });
    } else if regexs["MVRM"].is_match(ins) {
        println!("find Mvrm instruction: {}", ins);
        return Ok(Instruction::Mvrm{ dst: result[3].to_string(),
                                     src: result[7].to_string(),
                                     off: result[4].to_string() });
    } else if regexs["MVRR"].is_match(ins) {
        println!("find Mvrr instruction: {}", ins);
        return Ok(Instruction::Mvrr{ dst: result[1].to_string(),
                                     src: result[3].to_string() });
    } else if regexs["AROP"].is_match(ins) {
        println!("find Arop instruction: {}", ins);
        let operation = match result[2] {
            "+=" => Arithmetic::Add,
            "-=" => Arithmetic::Sub,
            "*=" => Arithmetic::Mul,
            "&=" => Arithmetic::And,
            _    => return Err(format!("[Syntax Error] Instruction: Invalid Arithmetic Operation: {}", ins)), 
        };
        return Ok(Instruction::Arop{ dst: result[1].to_string(),
                                     src: result[3].to_string(),
                                     op:  operation });
    } else if regexs["SFOP"].is_match(ins) {
        println!("find Sfop instruction: {}", ins);
        let operation = match result[2] {
            "<<=" => Shift::Left,
            ">>=" => Shift::Right,
            _     => return Err(format!("[Syntax Error] Instruction: Invalid Shift Operation: {}", ins)),
        };
        return Ok(Instruction::Sfop{ dst: result[1].to_string(),
                                     src: result[3].to_string(),
                                     op:  operation });
    } else if regexs["COMP"].is_match(ins) {
        println!("find Comp instruction: {}", ins);
        let operation = match result[4] {
            "<"  => Compare::Less,
            "<=" => Compare::Leeq,
            "="  => Compare::Eqal,
            _     => return Err(format!("[Syntax Error] Instruction: Invalid Compare Operation: {}", ins)),
        };
        return Ok(Instruction::Comp{ dst: result[1].to_string(),
                                     lhs: result[3].to_string(),
                                     rhs: result[5].to_string(),
                                     op:  operation });
    } else if regexs["LABL"].is_match(ins) {
        println!("find Label: {}", ins);
        return Ok(Instruction::Labl{ lab: result[0].to_string() });
    } else if regexs["GOTO"].is_match(ins) {
        println!("find Goto instruction: {}", ins);
        return Ok(Instruction::Goto{ dst: result[2].to_string() });
    } else if regexs["CJMP"].is_match(ins) {
        println!("find Cjmp instruction: {}", ins);
        let operation = match result[3] {
            "<"  => Compare::Less,
            "<=" => Compare::Leeq,
            "="  => Compare::Eqal,
            _     => return Err(format!("[Syntax Error] Instruction: Invalid Compare Operation: {}", ins)),
        };
        return Ok(Instruction::Cjmp{ lhs: result[2].to_string(),
                                     rhs: result[4].to_string(),
                                     op:  operation,
                                     tru: result[5].to_string(),
                                     fal: result[6].to_string() });
    } else if regexs["CALL"].is_match(ins) {
        println!("find Call instruction: {}", ins);
        return Ok(Instruction::Call{ dst: result[2].to_string(),
                                     arg: result[3].to_string() });
    } else if regexs["TCAL"].is_match(ins) {
        println!("find Tcal instruction: {}", ins);
        return Ok(Instruction::Tcal{ dst: result[2].to_string(),
                                     arg: result[3].to_string() });
    } else if regexs["RETN"].is_match(ins) {
        println!("find Retn instruction: {}", ins);
        return Ok(Instruction::Retn);
    } else if regexs["PRIT"].is_match(ins) {
        println!("find Prit instruction: {}", ins);
        return Ok(Instruction::Prit);
    } else if regexs["ALOC"].is_match(ins) {
        println!("find Aloc instruction: {}", ins);
        return Ok(Instruction::Aloc);
    } else if regexs["ARER"].is_match(ins) {
        println!("find Arer instruction: {}", ins);
        return Ok(Instruction::Arer);
    } else {
        return Err(format!("[Syntax Error] Instruction: Format Error: {}", ins));
    }
} 
