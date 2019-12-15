/* Day 6: Universal Orbit Map (Part 2) */

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;
use std::process;

fn trace(links: &HashMap<String, String>, start: &str) -> Vec<String> {
    let mut path: Vec<String> = vec![start.to_owned()];
    while let Some(next) = links.get(path.last().unwrap()) {
        path.push(next.to_owned());
    }
    path
}

#[allow(clippy::cast_precision_loss)]
fn main() {
    let mut links: HashMap<String, String> = HashMap::new();
    let mut handler: File = File::open(env::args().nth(1).unwrap()).unwrap();
    let mut contents = String::new();
    handler.read_to_string(&mut contents).unwrap();
    for pair in contents.trim_end().split('\n') {
        if let [a, b] = pair.split(')').collect::<Vec<&str>>()[..] {
            if links.insert(b.to_owned(), a.to_owned()).is_none() {
                continue;
            }
        }
        process::exit(1);
    }
    let mut a: Vec<String> = trace(&links, "YOU");
    let mut b: Vec<String> = trace(&links, "SAN");
    while a.last().unwrap() == b.last().unwrap() {
        a.pop();
        b.pop();
    }
    println!("{}", (a.len() - 1) + (b.len() - 1));
}
