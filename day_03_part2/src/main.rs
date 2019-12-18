/* Day 3: Crossed Wires (Part 2) | Answer: 14746 */

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;
use std::u32;

#[derive(Debug)]
enum Move {
    Up(u32),
    Down(u32),
    Left(u32),
    Right(u32),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Position {
    x: i16,
    y: i16,
}

fn parse(s: &str) -> Move {
    macro_rules! tail_to_int {
        () => {
            s[1..].parse::<u32>().unwrap()
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

macro_rules! iterate {
    ($moves:expr, $position:expr, $f:tt $(,)?) => {
        for m in $moves {
            match m {
                Move::Up(n) => {
                    for _ in 0..*n {
                        $position.y += 1;
                        $f!();
                    }
                }
                Move::Down(n) => {
                    for _ in 0..*n {
                        $position.y -= 1;
                        $f!();
                    }
                }
                Move::Left(n) => {
                    for _ in 0..*n {
                        $position.x -= 1;
                        $f!();
                    }
                }
                Move::Right(n) => {
                    for _ in 0..*n {
                        $position.x += 1;
                        $f!();
                    }
                }
            }
        }
    };
}

fn record_steps(moves: &[Move]) -> HashMap<Position, u32> {
    let mut memory: HashMap<Position, u32> = HashMap::new();
    let mut position: Position = Position { x: 0, y: 0 };
    let mut step: u32 = 0;
    macro_rules! advance {
        () => {
            step += 1;
            memory.entry(position.clone()).or_insert(step);
        };
    }
    iterate!(moves, position, advance);
    memory
}

fn find_intersections(memory: &HashMap<Position, u32>, moves: &[Move]) -> u32 {
    let mut result: u32 = u32::max_value();
    let mut position: Position = Position { x: 0, y: 0 };
    let mut step_second: u32 = 0;
    macro_rules! check_intersect {
        () => {
            step_second += 1;
            if let Some(step_first) = memory.get(&position) {
                let candidate: u32 = step_first + step_second;
                if candidate < result {
                    result = candidate;
                }
            }
        };
    };
    iterate!(moves, position, check_intersect);
    result
}

fn main() {
    if let [first, second] = &arg_to_moves()[..] {
        let memory: HashMap<Position, u32> = record_steps(first);
        println!("{:?}", find_intersections(&memory, second));
    }
}
