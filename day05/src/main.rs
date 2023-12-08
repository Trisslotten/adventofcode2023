use std::{collections::HashMap, fs};

#[derive(Clone, Copy, Debug)]
struct Range {
    source_min: i64,
    source_max: i64,
}
impl Range {
    fn contains(&self, value: i64) -> bool {
        value >= self.source_min && value <= self.source_max
    }
    fn intersects(&self, other: &Range) -> bool {
        self.source_min <= other.source_max && self.source_max >= other.source_min
    }
    fn offset(&mut self, offset: i64) {
        self.source_min += offset;
        self.source_max += offset;
    }
}

#[derive(Clone, Copy, Debug)]
struct OffsetRange {
    range: Range,
    offset: i64,
}
impl OffsetRange {
    fn contains(&self, value: i64) -> bool {
        self.range.contains(value)
    }
}

#[derive(Default)]
struct Map {
    ranges: Vec<OffsetRange>,
}
impl Map {
    fn get(&self, value: i64) -> i64 {
        for range in self.ranges.iter() {
            if range.contains(value) {
                return value + range.offset;
            }
        }
        value
    }
}

fn get_location(maps: &HashMap<&str, Map>, to_next_map: &HashMap<&str, &str>, seed: i64) -> i64 {
    let mut current_map_name = "seed-to-soil map:";
    let mut current_value = seed;

    loop {
        let current_map = maps.get(current_map_name).unwrap();
        current_value = current_map.get(current_value);

        if let Some(next_map_name) = to_next_map.get(current_map_name) {
            current_map_name = next_map_name;
        } else {
            return current_value;
        }
    }
}

fn get_min_location(
    maps: &HashMap<&str, Map>,
    to_next_map: &HashMap<&str, &str>,
    seed_ranges: Vec<Range>,
) -> i64 {
    let mut current_map_name = "seed-to-soil map:";

    let mut ranges: Vec<Range> = seed_ranges;

    println!("{ranges:?}");

    loop {
        let current_map = maps.get(current_map_name).unwrap();

        for map_range in current_map.ranges.iter().map(|r| &r.range) {
            // cut upp ranges
            let mut new_ranges: Vec<Range> = Vec::new();
            for r in ranges.iter() {
                let min_inside = r.contains(map_range.source_min);
                let max_inside = r.contains(map_range.source_max);
                let min_eq = r.source_min == map_range.source_min;
                let max_eq = r.source_max == map_range.source_max;

                if min_inside && max_inside {
                    if min_eq && max_eq {
                        new_ranges.push(*r);
                    } else if min_eq {
                        new_ranges.push(*map_range);
                        new_ranges.push(Range {
                            source_min: map_range.source_max + 1,
                            source_max: r.source_max,
                        });
                    } else if max_eq {
                        new_ranges.push(Range {
                            source_min: r.source_min,
                            source_max: map_range.source_min - 1,
                        });
                        new_ranges.push(*map_range);
                    } else {
                        new_ranges.push(Range {
                            source_min: r.source_min,
                            source_max: map_range.source_min - 1,
                        });
                        new_ranges.push(*map_range);
                        new_ranges.push(Range {
                            source_min: map_range.source_max + 1,
                            source_max: r.source_max,
                        });
                    }
                } else if min_inside {
                    if min_eq {
                        new_ranges.push(*r);
                    } else {
                        new_ranges.push(Range {
                            source_min: r.source_min,
                            source_max: map_range.source_min - 1,
                        });
                        new_ranges.push(Range {
                            source_min: map_range.source_min,
                            source_max: r.source_max,
                        });
                    }
                } else if max_inside {
                    if max_eq {
                        new_ranges.push(*r);
                    } else {
                        new_ranges.push(Range {
                            source_min: r.source_min,
                            source_max: map_range.source_max - 1,
                        });
                        new_ranges.push(Range {
                            source_min: map_range.source_max,
                            source_max: r.source_max,
                        });
                    }
                } else {
                    new_ranges.push(*r);
                }
            }
            ranges = new_ranges;
        }

        for r in ranges.iter_mut() {
            r.source_min = current_map.get(r.source_min);
            r.source_max = current_map.get(r.source_max);
        }

        // println!("{ranges:?}");

        if let Some(next_map_name) = to_next_map.get(current_map_name) {
            current_map_name = next_map_name;
        } else {
            // Done
            return ranges.iter().map(|x| x.source_min).min().unwrap();
        }
    }
}

fn parse_seeds_part1(lines: &mut std::str::Lines<'_>) -> Vec<i64> {
    let first_line = lines.next().unwrap();
    let mut seeds_split = first_line.split(": ");
    seeds_split.next();
    let seeds: Vec<_> = seeds_split
        .next()
        .unwrap()
        .split(' ')
        .map(|s| s.parse::<i64>().unwrap())
        .collect();
    seeds
}

fn parse_maps(lines: std::str::Lines<'_>) -> (HashMap<&str, &str>, HashMap<&str, Map>) {
    let mut maps: HashMap<&str, Map> = HashMap::new();
    let mut to_next_map: HashMap<&str, &str> = HashMap::new();
    let mut current_map = "";

    for line in lines {
        if line.is_empty() {
            continue;
        }

        let new_map = match line {
            "seed-to-soil map:"
            | "soil-to-fertilizer map:"
            | "fertilizer-to-water map:"
            | "water-to-light map:"
            | "light-to-temperature map:"
            | "temperature-to-humidity map:"
            | "humidity-to-location map:" => Some(line),
            _ => None,
        };

        if let Some(new_map) = new_map {
            if !current_map.is_empty() {
                to_next_map.insert(current_map, new_map);
            }
            current_map = new_map;
            maps.insert(current_map, Map::default());
        } else {
            let map = maps.get_mut(current_map).unwrap();

            let mut nums = line.split(' ').map(|s| s.parse::<i64>().unwrap());
            let dst_start = nums.next().unwrap();
            let src_start = nums.next().unwrap();
            let count = nums.next().unwrap();

            let offset = dst_start - src_start;
            let src_end = src_start + count - 1;

            map.ranges.push(OffsetRange {
                range: Range {
                    source_min: src_start,
                    source_max: src_end,
                },
                offset,
            });
        }
    }

    (to_next_map, maps)
}

fn part1(input: String) {
    let mut lines = input.lines();

    let seeds = parse_seeds_part1(&mut lines);

    let (to_next_map, maps) = parse_maps(lines);

    let min = seeds
        .iter()
        .map(|&s| get_location(&maps, &to_next_map, s))
        .min()
        .unwrap();
    println!("{min}");
}

fn parse_seeds_part2(lines: &mut std::str::Lines<'_>) -> Vec<Range> {
    let first_line = lines.next().unwrap();
    let mut seeds_split = first_line.split(": ");
    seeds_split.next();
    let seeds: Vec<_> = seeds_split
        .next()
        .unwrap()
        .split(' ')
        .map(|s| s.parse::<i64>().unwrap())
        .collect();

    let mut result: Vec<Range> = Vec::new();

    for chnk in seeds.chunks(2) {
        let start = chnk[0];
        let length = chnk[1];

        result.push(Range {
            source_min: start,
            source_max: start + length - 1,
        });
    }

    result
}

fn part2(input: String) {
    let mut lines = input.lines();

    let seed_ranges = parse_seeds_part2(&mut lines);

    let (to_next_map, maps) = parse_maps(lines);

    let min = get_min_location(&maps, &to_next_map, seed_ranges);

    println!("{min}");
}

fn main() {
    let contents = fs::read_to_string("input.txt").unwrap();
    part1(contents.clone());
    part2(contents);
}
