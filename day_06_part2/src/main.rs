/* Day 6: Universal Orbit Map (Part 2) | Answer: 283 */

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;
use std::process;

fn trace<'a>(
    links: &'a HashMap<&'a str, &'a str>,
    start: &'a str,
) -> Vec<&'a str> {
    let mut path: Vec<&str> = vec![start];
    while let Some(next) = links.get(path.last().unwrap()) {
        path.push(next);
    }
    path
}

fn main() {
    let mut links: HashMap<&str, &str> = HashMap::new();
    let mut handler: File = File::open(env::args().nth(1).unwrap()).unwrap();
    let mut contents = String::new();
    handler.read_to_string(&mut contents).unwrap();
    for pair in contents.trim_end().split('\n') {
        if let [a, b] = pair.split(')').collect::<Vec<&str>>()[..] {
            if links.insert(b, a).is_none() {
                continue;
            }
        }
        process::exit(1);
    }
    let mut a: Vec<&str> = trace(&links, "YOU");
    let mut b: Vec<&str> = trace(&links, "SAN");
    while a.last().unwrap() == b.last().unwrap() {
        a.pop();
        b.pop();
    }
    println!("{}", (a.len() - 1) + (b.len() - 1));
}
