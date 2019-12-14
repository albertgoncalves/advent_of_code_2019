use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum Move {
    Up(usize),
    Down(usize),
    Left(usize),
    Right(usize),
}

fn parse(s: &str) -> Move {
    macro_rules! tail_to_int {
        () => {
            s[1..].parse::<usize>().unwrap()
        };
    };
    match s.chars().next().unwrap() {
        'U' => Move::Up(tail_to_int!()),
        'D' => Move::Down(tail_to_int!()),
        'L' => Move::Left(tail_to_int!()),
        'R' => Move::Right(tail_to_int!()),
        _ => unreachable!(),
    }
}

fn arg_to_moves() -> Vec<Vec<Move>> {
    let mut handler: File = File::open(env::args().nth(1).unwrap()).unwrap();
    let mut contents = String::new();
    handler.read_to_string(&mut contents).unwrap();
    contents
        .trim_end()
        .split('\n')
        .collect::<Vec<&str>>()
        .iter()
        .map(|l| l.split(',').map(parse).collect::<Vec<Move>>())
        .collect::<Vec<Vec<Move>>>()
}

fn main() {
    if let [first, second] = &arg_to_moves()[..] {
        println!("{:?}", first);
        println!("{:?}", second);
    }
}
