/* Day 5: Sunny with a Chance of Asteroids (Part 1) | Answer: 5821753 */

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

fn main() {
    let mut xs: Vec<i32> = string_to_instructions(&stdin_to_string());
    let n: usize = xs.len();
    let mut i: usize = 0;
    xs[225] = 1;
    while (i <= n) && (xs[i] != 99) {
        match xs[i] % 10 {
            1 => {
                let j: usize = index(&xs, i, 3);
                xs[j] = xs[index(&xs, i, 1)] + xs[index(&xs, i, 2)];
                i += 4;
            }
            2 => {
                let j: usize = index(&xs, i, 3);
                xs[j] = xs[index(&xs, i, 1)] * xs[index(&xs, i, 2)];
                i += 4;
            }
            3 => i += 2,
            4 => {
                let j: usize = index(&xs, i, 1);
                println!("{}", xs[j]);
                i += 2;
            }
            _ => unreachable!(),
        }
    }
}
