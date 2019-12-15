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

#[allow(clippy::cast_sign_loss)]
fn calculate(mut xs: Vec<i32>, first: i32, second: i32) -> Option<i32> {
    let mut counter: u8 = 0;
    let n: usize = xs.len();
    let mut result: Option<i32> = None;
    let mut i: usize = 0;
    while (i <= n) && (xs[i] != 99) {
        match xs[i] % 10 {
            1 => {
                let j: usize = index(&xs, i, 3);
                xs[j] = xs[index(&xs, i, 1)] + xs[index(&xs, i, 2)];
                if i != j {
                    i += 4;
                }
            }
            2 => {
                let j: usize = index(&xs, i, 3);
                xs[j] = xs[index(&xs, i, 1)] * xs[index(&xs, i, 2)];
                if i != j {
                    i += 4;
                }
            }
            3 => {
                let j: usize = index(&xs, i, 1);
                if counter == 0 {
                    xs[j] = first;
                    counter += 1;
                } else if counter == 1 {
                    xs[j] = second;
                    counter += 1;
                }
                if i != j {
                    i += 2;
                }
            }
            4 => {
                let j: usize = index(&xs, i, 1);
                result = Some(xs[j]);
                if i != j {
                    i += 2;
                }
            }
            5 => {
                if xs[index(&xs, i, 1)] == 0 {
                    i += 3;
                } else {
                    i = xs[index(&xs, i, 2)] as usize;
                }
            }
            6 => {
                if xs[index(&xs, i, 1)] == 0 {
                    i = xs[index(&xs, i, 2)] as usize;
                } else {
                    i += 3;
                }
            }
            7 => {
                let j: usize = index(&xs, i, 3);
                if xs[index(&xs, i, 1)] < xs[index(&xs, i, 2)] {
                    xs[j] = 1;
                } else {
                    xs[j] = 0;
                }
                if i != j {
                    i += 4;
                }
            }
            8 => {
                let j: usize = index(&xs, i, 3);
                if xs[index(&xs, i, 1)] == xs[index(&xs, i, 2)] {
                    xs[j] = 1;
                } else {
                    xs[j] = 0;
                }
                if i != j {
                    i += 4;
                }
            }
            _ => unreachable!(),
        }
    }
    result
}

fn iterate(xs: &[i32], sequence: &[i32]) -> i32 {
    let mut second: i32 = 0;
    for first in sequence {
        if let Some(result) = calculate(xs.to_owned(), *first, second) {
            second = result;
        }
    }
    second
}

fn main() {
    let xs: Vec<i32> = string_to_instructions(&stdin_to_string());
    let mut sequence: [i32; 5] = [0, 1, 2, 3, 4];
    let mut result: i32 = 0;
    heap_recursive(&mut sequence, |s| {
        let candidate: i32 = iterate(&xs, s);
        if result < candidate {
            result = candidate;
        }
    });
    println!("{}", result);
}
