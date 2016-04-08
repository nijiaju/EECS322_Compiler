extern crate regex;

use std::fs::File;
use std::io::{Read, Write};
use std::env;
use std::str::{Chars, SplitWhitespace};
use regex::Regex;
use std::marker::Send;

#[derive(Debug)]
#[derive(Clone)]
enum Instruction<'a> {
    Load { regs: &'a str, offs: &'a str },
    Mvrr { dest: &'a str, sorc: &'a str },
    Mvmr { dest: &'a str, load: Box<Instruction<'a>> },
}

#[derive(Debug)]
enum Anything<'a> {
    Instruction(Instruction<'a>),
    RawWord(&'a str),
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

fn l1_instruction_parser<'a> (word_iter: &mut SplitWhitespace) -> Result<Instruction<'a>, String> {
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
            _   => words.push(Anything::RawWord(word)),
        }
    }
    println!("l1 parser {:?}", words);

    match words.len() {
        3 => {
            match (&words[0], &words[1], &words[2]) {
                (&Anything::RawWord("mem"), &Anything::RawWord(r), &Anything::RawWord(o)) =>
                    return Ok(Instruction::Load{regs: r, offs: o}),
                (&Anything::RawWord(d), &Anything::RawWord("<-"), &Anything::RawWord(s)) =>
                    return Ok(Instruction::Mvrr{dest: d, sorc: s}),
                (&Anything::RawWord(d), &Anything::RawWord("<-"), &Anything::Instruction(ref ins)) =>
                    return Ok(Instruction::Mvmr{dest: d, load: Box::new((*ins).clone())}),
                _ => {}
            }
        }
        _ => {}
    }

    return Err("".to_string());
}

/*
fn l1_word_parser(raw_word: &str) -> Result<Anything, String> {
    match raw_word {
        "+=" => return Ok(Anything::Operation(Operation::Add)),
        "<-" => return Ok(Anything::Operation(Operation::Assign)),
        _    => return Err("Unknow operation".to_string()),
    }

    let label_regex = Regex::new(r"^:[a-zA-Z_][a-zA-Z_0-9]*$").unwrap();
    let register_regex =
        Regex::new(r"^(rdi|rsi|rdx|rcx|r8|r9|rax|rbx|rbp|r10|r11|r12|r13|r14|r15|rsp)$").unwrap();
    let number_regex = Regex::new(r"^-?[0-9]+$").unwrap();
    let var_regex = Regex::new(r"^[a-zA-Z_][a-zA-Z_0-9-]*$").unwrap();
    
    if register_regex.is_match(raw_word) {
        return Ok(Anything::Register(raw_word.to_string()));
    } else if label_regex.is_match(raw_word) {
        return Ok(Anything::Label(raw_word.to_string()));
    } else if number_regex.is_match(raw_word) {
        return Ok(Anything::Number(raw_word.to_string()));
    } else if var_regex.is_match(raw_word) {
        return Ok(Anything::Variable(raw_word.to_string()));
    } else {
        return Err("[Syntax Error] Unknown Word".to_string());
    }
}
*/
