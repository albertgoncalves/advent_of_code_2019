/* Day 7: Amplification Circuit (Part 1) */

use permutohedron::heap_recursive;
use std::io::{self, Read};

fn stdin_to_string() -> String {
    let mut buffer: String = String::new();
    io::stdin().read_to_string(&mut buffer).unwrap();
    buffer
}

fn string_to_instructions(x: &str) -> Vec<i32> {
    x.trim_end()
        .split(',')
        .map(|y| y.parse::<i32>().unwrap())
        .collect::<Vec<i32>>()
}

#[allow(clippy::cast_sign_loss)]
fn index(xs: &[i32], i: usize, n: usize) -> usize {
    let mode: bool = match n {
        1 => (xs[i] / 100) % 10 == 0,
        2 => (xs[i] / 1000) % 10 == 0,
        3 => (xs[i] / 10000) % 10 == 0,
        _ => unreachable!(),
    };
    if mode {
        xs[i + n] as usize
    } else {
        i + n
    }
}

struct Memory {
    buffer: Vec<i32>,
    i: usize,
    result: Option<i32>,
}

fn init(xs: &[i32], i: usize, value: i32) -> Memory {
    let mut ys: Memory = Memory {
        buffer: xs.to_owned(),
        i: 2, /* Start *after* the first input instruction. */
        result: None,
    };
    ys.buffer[i] = value; /* Write first input instruction! */
    ys
}

#[allow(clippy::cast_sign_loss)]
fn calculate(memory: &mut Memory, value: i32) {
    let n: usize = memory.buffer.len();
    memory.result = None;
    let mut i: usize = memory.i;
    while (i <= n) && (memory.buffer[i] != 99) {
        match memory.buffer[i] % 10 {
            1 => {
                let j: usize = index(&memory.buffer, i, 3);
                memory.buffer[j] = memory.buffer[index(&memory.buffer, i, 1)]
                    + memory.buffer[index(&memory.buffer, i, 2)];
                if i != j {
                    i += 4;
                }
            }
            2 => {
                let j: usize = index(&memory.buffer, i, 3);
                memory.buffer[j] = memory.buffer[index(&memory.buffer, i, 1)]
                    * memory.buffer[index(&memory.buffer, i, 2)];
                if i != j {
                    i += 4;
                }
            }
            3 => {
                let j: usize = index(&memory.buffer, i, 1);
                memory.buffer[j] = value;
                /* Always move instruction pointer. */
                i += 2;
            }
            4 => {
                let j: usize = index(&memory.buffer, i, 1);
                if i != j {
                    i += 2;
                }
                memory.result = Some(memory.buffer[j]);
                memory.i = i;
                return;
            }
            5 => {
                if memory.buffer[index(&memory.buffer, i, 1)] == 0 {
                    i += 3;
                } else {
                    i = memory.buffer[index(&memory.buffer, i, 2)] as usize;
                }
            }
            6 => {
                if memory.buffer[index(&memory.buffer, i, 1)] == 0 {
                    i = memory.buffer[index(&memory.buffer, i, 2)] as usize;
                } else {
                    i += 3;
                }
            }
            7 => {
                let j: usize = index(&memory.buffer, i, 3);
                if memory.buffer[index(&memory.buffer, i, 1)]
                    < memory.buffer[index(&memory.buffer, i, 2)]
                {
                    memory.buffer[j] = 1;
                } else {
                    memory.buffer[j] = 0;
                }
                if i != j {
                    i += 4;
                }
            }
            8 => {
                let j: usize = index(&memory.buffer, i, 3);
                if memory.buffer[index(&memory.buffer, i, 1)]
                    == memory.buffer[index(&memory.buffer, i, 2)]
                {
                    memory.buffer[j] = 1;
                } else {
                    memory.buffer[j] = 0;
                }
                if i != j {
                    i += 4;
                }
            }
            _ => unreachable!(),
        }
    }
}

fn iterate(xs: &[i32], sequence: &[i32]) -> i32 {
    let i: usize = index(&xs, 0, 1);
    let mut memory_a: Memory = init(&xs, i, sequence[0]);
    let mut memory_b: Memory = init(&xs, i, sequence[1]);
    let mut memory_c: Memory = init(&xs, i, sequence[2]);
    let mut memory_d: Memory = init(&xs, i, sequence[3]);
    let mut memory_e: Memory = init(&xs, i, sequence[4]);
    let mut result: i32 = 0;
    loop {
        calculate(&mut memory_a, result);
        if memory_a.result.is_none() {
            break;
        }
        calculate(&mut memory_b, memory_a.result.unwrap());
        calculate(&mut memory_c, memory_b.result.unwrap());
        calculate(&mut memory_d, memory_c.result.unwrap());
        calculate(&mut memory_e, memory_d.result.unwrap());
        result = memory_e.result.unwrap();
    }
    result
}

fn main() {
    let xs: Vec<i32> = string_to_instructions(&stdin_to_string());
    let mut sequence: [i32; 5] = [5, 6, 7, 8, 9];
    let mut result: i32 = 0;
    heap_recursive(&mut sequence, |s| {
        let candidate: i32 = iterate(&xs, s);
        if result < candidate {
            result = candidate;
        }
    });
    println!("{}", result);
}
