/* Day 6: Universal Orbit Map (Part 1) | Answer: 142915 */

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;
use std::process;

#[allow(clippy::cast_precision_loss)]
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
    let mut depths: HashMap<&str, u32> = HashMap::new();
    let mut result: u32 = 0;
    let mut cost: u32 = 0;
    for key in links.keys() {
        let mut n: u32 = 0;
        let mut cursor: &str = key;
        while let Some(link) = links.get(cursor) {
            cost += 1;
            n += 1;
            result += 1;
            cursor = link;
            if let Some(m) = depths.get(cursor) {
                n += m;
                result += m;
                break;
            }
        }
        depths.insert(key, n);
    }
    println!(
        "{} {} {}",
        result,
        cost,
        1.0 - (cost as f32) / (result as f32),
    );
}
