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
fn calculate(
    xs: &mut Vec<i32>,
    mut i: usize,
    value: i32,
) -> Option<(usize, i32)> {
    let n: usize = xs.len();
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
                xs[j] = value;
                /* Always move instruction pointer. */
                i += 2;
            }
            4 => {
                let j: usize = index(&xs, i, 1);
                if i != j {
                    i += 2;
                }
                return Some((i, xs[j]));
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
    None
}

#[allow(clippy::similar_names)]
fn iterate(xs: &[i32], sequence: &[i32]) -> i32 {
    let i: usize = index(&xs, 0, 1);
    let mut xs_a: Vec<i32> = xs.to_owned();
    xs_a[i] = sequence[0];
    let mut xs_b: Vec<i32> = xs.to_owned();
    xs_b[i] = sequence[1];
    let mut xs_c: Vec<i32> = xs.to_owned();
    xs_c[i] = sequence[2];
    let mut xs_d: Vec<i32> = xs.to_owned();
    xs_d[i] = sequence[3];
    let mut xs_e: Vec<i32> = xs.to_owned();
    xs_e[i] = sequence[4];
    let mut i_a: usize = 2;
    let mut i_b: usize = 2;
    let mut i_c: usize = 2;
    let mut i_d: usize = 2;
    let mut i_e: usize = 2;
    let mut value: i32 = 0;
    loop {
        let out_a = calculate(&mut xs_a, i_a, value);
        if out_a.is_none() {
            break;
        }
        let (index_a, x_a): (usize, i32) = out_a.unwrap();
        i_a = index_a;
        let (index_b, x_b): (usize, i32) =
            calculate(&mut xs_b, i_b, x_a).unwrap();
        i_b = index_b;
        let (index_c, x_c): (usize, i32) =
            calculate(&mut xs_c, i_c, x_b).unwrap();
        i_c = index_c;
        let (index_d, x_d): (usize, i32) =
            calculate(&mut xs_d, i_d, x_c).unwrap();
        i_d = index_d;
        if let Some((index_e, x_e)) = calculate(&mut xs_e, i_e, x_d) {
            i_e = index_e;
            value = x_e;
        } else {
            break;
        }
    }
    value
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
