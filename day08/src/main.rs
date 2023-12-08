use std::{collections::HashMap, fs};

fn parse_map(lines: std::str::Lines<'_>) -> HashMap<&str, (&str, &str)> {
    let mut map: HashMap<&str, (&str, &str)> = HashMap::new();
    for line in lines {
        let (key, rest) = line.split_at(3);
        let left = rest.get(4..(4 + 3)).unwrap();
        let right = rest.get(9..(9 + 3)).unwrap();
        map.insert(key, (left, right));
    }
    map
}

fn parse_input(input: &str) -> (&str, HashMap<&str, (&str, &str)>) {
    let mut lines = input.lines();
    let walk = lines.next().unwrap();
    lines.next();
    let map = parse_map(lines);
    (walk, map)
}

fn part1(input: &str) {
    let (walk, map) = parse_input(input);

    let mut current = "AAA";
    let walk_len = walk.len();
    let mut final_steps = 0;
    for steps in 0.. {
        let current_walk = walk.as_bytes()[steps % walk_len] as char;

        let (left, right) = map.get(current).unwrap();

        // println!("At {current} -> ({left}, {right}), going to {current_walk}");

        if current_walk == 'L' {
            current = left;
        } else {
            current = right;
        }

        if current == "ZZZ" {
            final_steps = steps + 1;
            break;
        }
    }

    println!("{final_steps}");
}

#[derive(Debug)]
struct Walk<'a> {
    current: &'a str,
    initial_offset: Option<i64>,
    loop_size: Option<i64>,
}

fn part2(input: &str) {
    let (dirs, map) = parse_input(input);

    let mut walks: Vec<Walk> = Vec::new();
    for key in map.keys() {
        if key.as_bytes()[2] == b'A' {
            walks.push(Walk {
                current: key,
                initial_offset: None,
                loop_size: None,
            });
        }
    }

    let dirs_len = dirs.len();
    for steps in 0.. {
        let direction = dirs.as_bytes()[steps % dirs_len] as char;
        for walk in walks.iter_mut() {
            let (left, right) = map.get(walk.current).unwrap();

            if direction == 'L' {
                walk.current = left;
            } else {
                walk.current = right;
            }

            if walk.current.as_bytes()[2] == b'Z' {
                if let Some(init_offset) = walk.initial_offset {
                    if walk.loop_size.is_none() {
                        walk.loop_size = Some(steps as i64 + 1 - init_offset);
                    }
                } else {
                    walk.initial_offset = Some(steps as i64 + 1);
                }
            }
        }

        if walks.iter().all(|w| w.loop_size.is_some()) {
            break;
        }
    }

    for walk in walks.iter_mut() {
        let num = walk.initial_offset.unwrap();
        println!("{num}");
    }
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    // part1(&input);
    part2(&input);
}
