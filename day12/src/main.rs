/*
Task description:

You finally reach the hot springs! You can see steam rising from secluded areas attached to the primary, ornate building.

As you turn to enter, the researcher stops you. "Wait - I thought you were looking for the hot springs, weren't you?" You indicate that this definitely looks like hot springs to you.

"Oh, sorry, common mistake! This is actually the onsen! The hot springs are next door."

You look in the direction the researcher is pointing and suddenly notice the massive metal helixes towering overhead. "This way!"

It only takes you a few more steps to reach the main gate of the massive fenced-off area containing the springs. You go through the gate and into a small administrative building.

"Hello! What brings you to the hot springs today? Sorry they're not very hot right now; we're having a lava shortage at the moment." You ask about the missing machine parts for Desert Island.

"Oh, all of Gear Island is currently offline! Nothing is being manufactured at the moment, not until we get more lava to heat our forges. And our springs. The springs aren't very springy unless they're hot!"

"Say, could you go up and see why the lava stopped flowing? The springs are too cold for normal operation, but we should be able to find one springy enough to launch you up there!"

There's just one problem - many of the springs have fallen into disrepair, so they're not actually sure which springs would even be safe to use! Worse yet, their condition records of which springs are damaged (your puzzle input) are also damaged! You'll need to help them repair the damaged records.

In the giant field just outside, the springs are arranged into rows. For each row, the condition records show every spring and whether it is operational (.) or damaged (#). This is the part of the condition records that is itself damaged; for some springs, it is simply unknown (?) whether the spring is operational or damaged.

However, the engineer that produced the condition records also duplicated some of this information in a different format! After the list of springs for a given row, the size of each contiguous group of damaged springs is listed in the order those groups appear in the row. This list always accounts for every damaged spring, and each number is the entire size of its contiguous group (that is, groups are always separated by at least one operational spring: #### would always be 4, never 2,2).

So, condition records with no unknown spring conditions might look like this:

#.#.### 1,1,3
.#...#....###. 1,1,3
.#.###.#.###### 1,3,1,6
####.#...#... 4,1,1
#....######..#####. 1,6,5
.###.##....# 3,2,1

However, the condition records are partially damaged; some of the springs' conditions are actually unknown (?). For example:

???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1

Equipped with this information, it is your job to figure out how many different arrangements of operational and broken springs fit the given criteria in each row.

In the first line (???.### 1,1,3), there is exactly one way separate groups of one, one, and three broken springs (in that order) can appear in that row: the first three unknown springs must be broken, then operational, then broken (#.#), making the whole row #.#.###.

The second line is more interesting: .??..??...?##. 1,1,3 could be a total of four different arrangements. The last ? must always be broken (to satisfy the final contiguous group of three broken springs), and each ?? must hide exactly one of the two broken springs. (Neither ?? could be both broken springs or they would form a single contiguous group of two; if that were true, the numbers afterward would have been 2,3 instead.) Since each ?? can either be #. or .#, there are four possible arrangements of springs.

The last line is actually consistent with ten different arrangements! Because the first number is 3, the first and second ? must both be . (if either were #, the first number would have to be 4 or higher). However, the remaining run of unknown spring conditions have many different ways they could hold groups of two and one broken springs:

?###???????? 3,2,1
.###.##.#...
.###.##..#..
.###.##...#.
.###.##....#
.###..##.#..
.###..##..#.
.###..##...#
.###...##.#.
.###...##..#
.###....##.#

In this example, the number of possible arrangements for each row is:

    ???.### 1,1,3 - 1 arrangement
    .??..??...?##. 1,1,3 - 4 arrangements
    ?#?#?#?#?#?#?#? 1,3,1,6 - 1 arrangement
    ????.#...#... 4,1,1 - 1 arrangement
    ????.######..#####. 1,6,5 - 4 arrangements
    ?###???????? 3,2,1 - 10 arrangements

Adding all of the possible arrangement counts together produces a total of 21 arrangements.

For each row, count all of the different arrangements of operational and broken springs that meet the given criteria. What is the sum of those counts?

--- Part Two ---

As you look out at the field of springs, you feel like there are way more springs than the condition records list. When you examine the records, you discover that they were actually folded up this whole time!

To unfold the records, on each row, replace the list of spring conditions with five copies of itself (separated by ?) and replace the list of contiguous groups of damaged springs with five copies of itself (separated by ,).

So, this row:

.# 1

Would become:

.#?.#?.#?.#?.# 1,1,1,1,1

The first line of the above example would become:

???.###????.###????.###????.###????.### 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3

In the above example, after unfolding, the number of possible arrangements for some rows is now much larger:

    ???.### 1,1,3 - 1 arrangement
    .??..??...?##. 1,1,3 - 16384 arrangements
    ?#?#?#?#?#?#?#? 1,3,1,6 - 1 arrangement
    ????.#...#... 4,1,1 - 16 arrangements
    ????.######..#####. 1,6,5 - 2500 arrangements
    ?###???????? 3,2,1 - 506250 arrangements

After unfolding, adding all of the possible arrangement counts together produces 525152.

Unfold your condition records; what is the new sum of possible arrangement counts?


*/

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum Spring {
    Unknown,
    Broken,
    Operational,
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct SprintSegment {
    length: i64,
    spring: Spring,
}

fn parse_spring(c: char) -> Spring {
    match c {
        '?' => Spring::Unknown,
        '#' => Spring::Broken,
        '.' => Spring::Operational,
        _ => panic!("unexpected character: {}", c),
    }
}

fn is_valid(springs: &str, nums: &[usize]) -> bool {
    let spring_segments = parse_segments(springs);
    // clone spring_segments excluding Operational springs
    let spring_segments: Vec<SprintSegment> = spring_segments
        .iter()
        .filter(|s| s.spring != Spring::Operational)
        .cloned()
        .collect();

    // println!("spring_segments: {:?}", spring_segments);
    // println!("nums: {:?}", nums);

    if spring_segments.len() != nums.len() {
        return false;
    }
    for (segment, num) in spring_segments.iter().zip(nums.iter()) {
        if segment.length != *num as i64 {
            return false;
        }
    }
    true
}

fn part1(input: &str) {
    let mut total_valid = 0;

    for line in input.lines() {
        let (springs, nums) = line.split_once(' ').unwrap();
        let nums: Vec<usize> = nums.split(',').map(|n| n.parse().unwrap()).collect();

        let mut curr: String = String::new();

        let mut num_valid = 0;

        let num_unknown = springs.chars().filter(|c| *c == '?').count() as u32;
        for i in 0..(2u32.pow(num_unknown)) {
            curr.clear();

            let mut curr_index = 0;
            for c in springs.chars() {
                if c == '?' {
                    let bit = (i >> curr_index) & 1;
                    if bit == 1 {
                        curr.push('#');
                    } else {
                        curr.push('.');
                    }
                    curr_index += 1;
                } else {
                    curr.push(c);
                }
            }

            if is_valid(&curr, &nums) {
                num_valid += 1;
                // println!("  valid: {}", curr);
            } else {
                // println!("invalid: {}", curr);
            }
        }

        total_valid += num_valid;
    }

    println!("total valid: {}", total_valid);
}

fn is_possibly_valid(
    springs: &str,
    nums: &[usize],
    spring_segments: &mut Vec<SprintSegment>,
) -> Option<(usize, usize)> {
    spring_segments.clear();
    for c in springs.chars() {
        let current = parse_spring(c);
        if let Some(last) = spring_segments.last_mut() {
            if last.spring == current {
                last.length += 1;
                continue;
            }
        }
        spring_segments.push(SprintSegment {
            length: 1,
            spring: current,
        });
    }

    let mut segments = spring_segments.iter();
    // .filter(|s| s.spring != Spring::Operational);
    let mut num_chars = 0;

    let mut num_i = 0;
    loop {
        let segment = segments.next();
        if segment.is_none() {
            return None;
        }
        let segment = segment.unwrap();
        num_chars += segment.length as usize;

        if segment.spring == Spring::Operational {
            continue;
        } else if segment.spring == Spring::Unknown {
            break;
            // return Some((num_chars - segment.length as usize, num_i));
        }

        let num: &usize = nums.get(num_i).unwrap();
        if segment.length > *num as i64 {
            return None;
        }
        num_i += 1;
    }

    // Fix problem where we don't know previous broken

    // for (num_segments, (segment, num)) in spring_segments.zip(nums.iter()).enumerate() {
    //     if segment.spring == Spring::Unknown {
    //         return Some((num_chars, num_segments));
    //     }
    //     num_chars += segment.length as usize;
    //     if segment.length > *num as i64 {
    //         return None;
    //     }
    // }

    Some((num_chars, nums.len()))
}

// fn part1v2(input: &str) {
//     let mut total_valid = 0;
//     let mut spring_segments: Vec<SprintSegment> = Vec::new();
//     spring_segments.reserve(20);
//     for line in input.lines() {
//         let (springs, nums) = line.split_once(' ').unwrap();
//         let nums: Vec<usize> = nums.split(',').map(|n| n.parse().unwrap()).collect();
//         let mut stack: Vec<String> = Vec::new();
//         stack.push(springs.to_string());
//         while let Some(mut curr) = stack.pop() {
//             if curr.contains('?') {
//                 let mut curr_index = 0;
//                 for c in curr.chars() {
//                     if c == '?' {
//                         let mut curr1 = curr.clone();
//                         let mut curr2 = curr.clone();
//                         curr1.replace_range(curr_index..curr_index + 1, "#");
//                         if is_possibly_valid(&curr1, &nums, &mut spring_segments) {
//                             // println!("\ncurr1: {}", curr1);
//                             stack.push(curr1);
//                         }
//                         curr2.replace_range(curr_index..curr_index + 1, ".");
//                         if is_possibly_valid(&curr2, &nums, &mut spring_segments) {
//                             // println!("curr2: {}", curr2);
//                             stack.push(curr2);
//                         }
//                         break;
//                     }
//                     curr_index += 1;
//                 }
//             } else {
//                 if is_valid(&curr, &nums) {
//                     total_valid += 1;
//                 }
//             }
//         }
//     }
//     println!("total valid: {}", total_valid);
// }

fn part2(input: &str) {
    let mut total_valid = 0;

    let mut spring_segments: Vec<SprintSegment> = Vec::new();
    spring_segments.reserve(20);

    for line in input.lines() {
        let (springs, nums) = line.split_once(' ').unwrap();
        let nums: Vec<usize> = nums.split(',').map(|n| n.parse().unwrap()).collect();

        // let mut new_springs = springs.to_string();
        // for _ in 0..4 {
        //     new_springs.push('?');
        //     new_springs.push_str(springs);
        // }
        // let springs = new_springs;
        // let nums = nums.repeat(5);
        println!("springs: {}", springs);
        println!("nums: {:?}", nums);

        let mut stack: Vec<(String, usize)> = Vec::new();
        stack.push((springs.to_string(), 0));
        while let Some((mut curr, segment_index)) = stack.pop() {
            let sub_nums = nums.get(segment_index..).unwrap();

            println!("Checking curr: {}, {:?}", curr, sub_nums);

            let mut curr_index = 0;
            for c in curr.chars() {
                if c == '?' {
                    let mut curr1 = curr.clone();
                    let mut curr2 = curr.clone();

                    curr1.replace_range(curr_index..curr_index + 1, "#");
                    println!("curr1: {}, {:?}", curr1, sub_nums);
                    if !curr1.contains('?') {
                        if is_valid(&curr1, &sub_nums) {
                            println!("\tvalid: {}", curr1);
                            total_valid += 1;
                        }
                    } else if let Some((num_chars, num_segments)) =
                        is_possibly_valid(&curr1, sub_nums, &mut spring_segments)
                    {
                        let curr1 = curr1.get(num_chars..).unwrap().to_string();
                        println!("\tpos valid: {}", curr1);
                        stack.push((curr1, num_segments));
                    }

                    curr2.replace_range(curr_index..curr_index + 1, ".");
                    println!("curr2: {}, {:?}", curr2, sub_nums);
                    if !curr2.contains('?') {
                        if is_valid(&curr2, &sub_nums) {
                            println!("\tvalid: {}", curr2);
                            total_valid += 1;
                        }
                    } else if let Some((num_chars, num_segments)) =
                        is_possibly_valid(&curr2, sub_nums, &mut spring_segments)
                    {
                        let curr2 = curr2.get(num_chars..).unwrap().to_string();
                        println!("\tpos valid: {}", curr2);
                        stack.push((curr2, num_segments));
                    }
                    break;
                }
                curr_index += 1;
            }
        }
    }

    // Idea: don't process beginning of string since we already know it's possibly valid

    println!("total valid: {}", total_valid);
}

fn parse_segments(springs: &str) -> Vec<SprintSegment> {
    let mut spring_segments: Vec<SprintSegment> = Vec::new();
    for c in springs.chars() {
        let current = parse_spring(c);
        if let Some(last) = spring_segments.last_mut() {
            if last.spring == current {
                last.length += 1;
                continue;
            }
        }
        spring_segments.push(SprintSegment {
            length: 1,
            spring: current,
        });
    }
    spring_segments
}

#[derive(Debug, Clone)]
struct StackElement {
    // springs: String,
    spring_index: usize,
    nums_index: usize,
    group_count: usize,
    prev_c: Option<char>,
    // debug_string: String,
}

fn part2v2(input: &str) {
    let mut total_valid = 0;

    let mut stack: Vec<StackElement> = Vec::new();
    stack.reserve(100);

    for line in input.lines() {
        let (loaded_springs, nums) = line.split_once(' ').unwrap();
        let nums: Vec<usize> = nums.split(',').map(|n| n.parse().unwrap()).collect();

        let mut new_springs = loaded_springs.to_string();
        for _ in 0..4 {
            new_springs.push('?');
            new_springs.push_str(loaded_springs);
        }
        let nums = nums.repeat(5);

        let mut springs = new_springs;
        springs.push('.');

        stack.clear();

        stack.push(StackElement {
            spring_index: 0,
            nums_index: 0,
            group_count: 0,
            prev_c: None,
        });

        println!("line: {}", line);

        let mut debug_stack: Vec<String> = Vec::new();
        debug_stack.push(String::new());

        while let Some(mut element) = stack.pop() {
            let mut debug_string = debug_stack.pop().unwrap();

            println!("{debug_string}");

            for c in springs.chars().skip(element.spring_index) {
                let _current_spring_index = element.spring_index;
                element.spring_index += 1;

                match c {
                    '?' => {
                        if let Some(prev_c) = element.prev_c {
                            if prev_c == '#' {
                                if element.group_count < nums[element.nums_index] {
                                    element.group_count += 1;
                                    element.prev_c = Some('#');
                                    debug_string.push('#');
                                } else if element.group_count > nums[element.nums_index] {
                                    break;
                                } else {
                                    element.nums_index += 1;
                                    element.prev_c = Some('.');
                                    debug_string.push('.');
                                }
                            } else if prev_c == '.' {
                                if element.nums_index < nums.len() {
                                    stack.push(StackElement {
                                        prev_c: Some('#'),
                                        group_count: 1,
                                        ..element
                                    });
                                    stack.push(StackElement {
                                        prev_c: Some('.'),
                                        group_count: 0,
                                        ..element
                                    });

                                    push_debug_str(&debug_string, &mut debug_stack, '#');
                                    push_debug_str(&debug_string, &mut debug_stack, '.');
                                    break;
                                } else {
                                    element.prev_c = Some('.');
                                    debug_string.push('.');
                                }
                            }
                        } else {
                            stack.push(StackElement {
                                prev_c: Some('#'),
                                group_count: 1,
                                ..element
                            });
                            stack.push(StackElement {
                                prev_c: Some('.'),
                                group_count: 0,
                                ..element
                            });

                            push_debug_str(&debug_string, &mut debug_stack, '#');
                            push_debug_str(&debug_string, &mut debug_stack, '.');
                            break;
                        }
                    }
                    '#' => {
                        if let Some(prev_c) = element.prev_c {
                            if prev_c == '.' {
                                element.group_count = 0;

                                if element.nums_index >= nums.len() {
                                    break;
                                }
                            }
                        }
                        element.group_count += 1;
                        element.prev_c = Some(c);

                        if element.group_count > nums[element.nums_index] {
                            break;
                        }

                        debug_string.push(c);
                    }
                    '.' => {
                        if let Some(prev_c) = element.prev_c {
                            if prev_c == '#' {
                                if element.group_count != nums[element.nums_index] {
                                    break;
                                }
                                element.nums_index += 1;
                            }
                        }
                        element.prev_c = Some(c);

                        debug_string.push(c);
                    }
                    _ => panic!("unexpected character: {}", c),
                }
            }

            if element.spring_index == springs.len() && element.nums_index == nums.len() {
                total_valid += 1;
            }
        }
    }

    println!("total valid: {}", total_valid);
}

fn push_debug_str(debug_string: &String, debug_stack: &mut Vec<String>, c: char) {
    let mut debugstr = debug_string.clone();
    debugstr.push(c);
    debug_stack.push(debugstr);
}

fn main() {
    // read from a file called input.txt
    let input = std::fs::read_to_string("input.txt").unwrap();

    // let start = std::time::Instant::now();
    // part1v2(input.as_str());
    // let duration = start.elapsed();
    // println!("Time elapsed in part1v2 is: {:?}", duration);

    let start = std::time::Instant::now();
    part2v2(input.as_str());
    let duration = start.elapsed();
    println!("Time elapsed in part2v2 is: {:?}", duration);
}
