use std::fs::File;
use std::io::{Read, Write};
use std::env;
use std::str::{Chars, SplitWhitespace};

#[derive(Debug)]
enum Instruction {
    Load { regs: String, offs: String },
    Mvrr { dest: String, sorc: String },
    Mvmr { dest: String, load: Box<Instruction> },
}

#[derive(Debug)]
enum Operation {
    Add,
    Sub,
    Muliply,
    And,
    Assign,
    ShiftLeft,
    ShiftRight,
    Less,
    LessEqual,
    Equal,
}

#[derive(Debug)]
enum Anything {
    Instruction(Instruction),
    Register(String),
    Number(String),
    Label(String),
    Operation(Operation),
    Variable(String),
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

    // preprocess

    // get an iterator to each "wrod"
    let mut codes = String::new();
    if let Err(e) = f.read_to_string(&mut codes) {
        println!("{}", e);
        return;
    }

    println!("{:?}", codes);
    let mut word_iter = codes.split_whitespace();

    let mut result = Vec::new();
    while let Some(word) = word_iter.next() {
        if word == "(" {
            result.push(l1_instruction_parser(&mut word_iter));
        }
    }

    println!("{:?}", result);
    
}

fn l1_instruction_parser(word_iter: &mut SplitWhitespace) -> Result<Instruction, String> {
    let mut words = Vec::new();
    while let Some(word) = word_iter.next() {
        println!("l1 ins parser: {}", word);
        match word {
            "(" => 
                match l1_instruction_parser(word_iter) {
                    Ok(ins) => words.push(Anything::Instruction(ins)),
                    Err(e)  => return Err(e),
                },
            ")" => break,
            _   => words.push(Anything::RawWord(word.to_string())),
        }
    }
    println!("l1 parser {:?}", words);


    return Err("".to_string());
}

fn l1_word_parser(raw_word: &str) -> Anything {
    match raw_word {
        "+="
