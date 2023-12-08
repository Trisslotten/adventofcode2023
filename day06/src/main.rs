use std::fs;

struct Race {
    time: i64,
    distance: i64,
}

fn parse(input: &str) -> Vec<Race> {
    let mut lines = input.lines();
    let (_, times_str) = lines.next().unwrap().split_at(11);
    let (_, distances_str) = lines.next().unwrap().split_at(11);

    let times: Vec<_> = times_str
        .split(' ')
        .filter(|t| !t.is_empty())
        .map(|t| t.parse::<i64>().unwrap())
        .collect();

    let distances: Vec<_> = distances_str
        .split(' ')
        .filter(|t| !t.is_empty())
        .map(|t| t.parse::<i64>().unwrap())
        .collect();

    let mut result: Vec<Race> = Vec::new();
    for (i, t) in times.iter().enumerate() {
        result.push(Race {
            time: *t,
            distance: distances[i],
        });
    }

    result
}

fn part1(input: &str) {
    let races = parse(input);
    
    let mut values: Vec<i64> = Vec::new();

    for race in races.iter() {
        
        let mut value = 0;
        let mut found = false;
        for hold_time in 1.. {
            let velocity = hold_time;
            let travel_time = race.distance / velocity;
            let total_time = hold_time + travel_time;
            if total_time < race.time {
                found = true;
                value += 1;
            } else if found {
                break;
            }
        }
        values.push(value);
        // let hold_time = 0;
        // let time_spent = hold_time + velocity *

        // 1/v = t/s
        // v = s/t
    }

    let result: i64 = values.iter().product();
    println!("{result}")
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    part1(&input);
}
