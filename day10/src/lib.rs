use std::collections::VecDeque;
#[derive(Debug)]
struct Map {
    map: Vec<char>,
    width: i64,
    height: i64,
}
// An impl for Map that has a function to get an item from an x and y
impl Map {
    fn get(&self, x: i64, y: i64) -> char {
        if x < 0 || y < 0 || x >= self.width || y >= self.height {
            '.'
        } else {
            self.map[(y * self.width + x) as usize]
        }
    }
    // function to get starting x and y
    fn get_start(&self) -> (i64, i64) {
        for y in 0..self.height {
            for x in 0..self.width {
                if self.get(x, y) == 'S' {
                    return (x, y);
                }
            }
        }
        (0, 0)
    }

    fn get_index(&self, x: i64, y: i64) -> usize {
        (y * self.width + x) as usize
    }

    fn can_move_up_to(&self, (x, y): (i64, i64)) -> bool {
        let from = self.get(x, y + 1);
        let to = self.get(x, y);
        are_connected(from, to, Direction::Up)
    }

    fn can_move_down_to(&self, (x, y): (i64, i64)) -> bool {
        let from = self.get(x, y - 1);
        let to = self.get(x, y);
        are_connected(from, to, Direction::Down)
    }

    fn can_move_left_to(&self, (x, y): (i64, i64)) -> bool {
        let from = self.get(x + 1, y);
        let to = self.get(x, y);
        are_connected(from, to, Direction::Left)
    }

    fn can_move_right_to(&self, (x, y): (i64, i64)) -> bool {
        let from = self.get(x - 1, y);
        let to = self.get(x, y);
        are_connected(from, to, Direction::Right)
    }
    fn for_each_neighbor<F>(&self, (x, y): (i64, i64), mut f: F)
    where
        F: FnMut((i64, i64)),
    {
        // Left
        if self.can_move_left_to((x - 1, y)) {
            f((x - 1, y));
        }
        // Right
        if self.can_move_right_to((x + 1, y)) {
            f((x + 1, y));
        }
        // Up
        if self.can_move_up_to((x, y - 1)) {
            f((x, y - 1));
        }
        // Down
        if self.can_move_down_to((x, y + 1)) {
            f((x, y + 1));
        }
    }
    fn for_each_neighbor_with_continue<F>(&self, (x, y): (i64, i64), mut f: F)
    where
        F: FnMut((i64, i64)) -> bool,
    {
        // Left
        if self.can_move_left_to((x - 1, y)) && f((x - 1, y)) {
            return;
        }
        // Right
        if self.can_move_right_to((x + 1, y)) && f((x + 1, y)) {
            return;
        }
        // Up
        if self.can_move_up_to((x, y - 1)) && f((x, y - 1)) {
            return;
        }
        // Down
        if self.can_move_down_to((x, y + 1)) && f((x, y + 1)) {
            return;
        }
    }
}

#[derive(PartialEq, Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
    None,
}

impl Direction {
    fn opposite(&self) -> Direction {
        match self {
            Direction::Up => Direction::Down,
            Direction::Down => Direction::Up,
            Direction::Left => Direction::Right,
            Direction::Right => Direction::Left,
            Direction::None => Direction::None,
        }
    }
}

fn get_connections(chr: char) -> [Direction; 2] {
    match chr {
        'S' => [Direction::None, Direction::None],
        'F' => [Direction::Down, Direction::Right],
        'J' => [Direction::Up, Direction::Left],
        'L' => [Direction::Up, Direction::Right],
        '7' => [Direction::Down, Direction::Left],
        '|' => [Direction::Up, Direction::Down],
        '-' => [Direction::Left, Direction::Right],
        _ => panic!("invalid chr '{}'", chr),
    }
}

fn are_connected(from: char, to: char, direction: Direction) -> bool {
    if from == '.' || to == '.' {
        return false;
    }

    let from_connections = get_connections(from);
    let to_connections = get_connections(to);

    if to == 'S' && from_connections.contains(&direction)
        || from == 'S' && to_connections.contains(&direction.opposite())
    {
        return true;
    }

    if !from_connections.contains(&direction) {
        return false;
    }
    if !to_connections.contains(&direction.opposite()) {
        return false;
    }

    true
}

fn parse_map(input: &str) -> Map {
    let map = Map {
        map: input.chars().filter(|c| *c != '\n').collect(),
        width: input.find('\n').unwrap() as i64,
        height: input.chars().filter(|c| *c == '\n').count() as i64 + 1,
    };
    map
}

pub fn part1(input: &str) -> String {
    // Create a Map
    let map = parse_map(input);
    let start = map.get_start();

    let mut queue = VecDeque::new();
    let mut visited = vec![false; (map.width * map.height) as usize];
    let mut max_distance = 0;

    queue.push_back((start, 0));
    visited[(start.1 * map.width + start.0) as usize] = true;

    while let Some((current, distance)) = queue.pop_front() {
        if distance > max_distance {
            max_distance = distance;
        }

        map.for_each_neighbor(current, |neighbor| {
            let index = (neighbor.1 * map.width + neighbor.0) as usize;
            if !visited[index] {
                visited[index] = true;
                queue.push_back((neighbor, distance + 1));
            }
        });
    }

    max_distance.to_string()
}

pub fn part2(input: &str) -> String {
    let map = parse_map(input);
    let start = map.get_start();

    let mut path = Vec::new();
    path.push(start);
    path.reserve(map.map.len());

    let mut visited: Vec<bool> = vec![false; (map.width * map.height) as usize];

    let mut prev = None;
    let mut current = start;

    loop {
        // println!(
        //     "current: {:?}, '{}'",
        //     current,
        //     map.get(current.0, current.1)
        // );

        let mut next = current;

        map.for_each_neighbor_with_continue(current, |neighbor| {
            // println!(
            //     "\tneighbor: {:?}, '{}'",
            //     neighbor,
            //     map.get(neighbor.0, neighbor.1)
            // );
            if prev.is_none() || neighbor != prev.unwrap() {
                // println!("\t\ttaking path");
                next = neighbor;
                path.push(neighbor);

                visited[(neighbor.1 * map.width + neighbor.0) as usize] = true;
                return true;
            }
            false
        });

        prev = Some(current);
        current = next;

        if prev.unwrap() == current {
            // println!("warning: stuck");
            break;
        }

        if current == start {
            break;
        }
    }

    let from = path[0];
    let to = path[1];
    let dir: Direction = match (to.0 - from.0, to.1 - from.1) {
        (0, -1) => Direction::Up,
        (0, 1) => Direction::Down,
        (-1, 0) => Direction::Left,
        (1, 0) => Direction::Right,
        _ => panic!("invalid delta"),
    };
    let mut prev_dir = dir;

    let mut winding_number: i64 = 0;

    for i in path.windows(2) {
        let from = i[0];
        let to = i[1];
        let dir = match (to.0 - from.0, to.1 - from.1) {
            (0, -1) => Direction::Up,
            (0, 1) => Direction::Down,
            (-1, 0) => Direction::Left,
            (1, 0) => Direction::Right,
            _ => panic!("invalid delta"),
        };

        match (prev_dir, dir) {
            (Direction::Up, Direction::Right) => winding_number += 1,
            (Direction::Right, Direction::Down) => winding_number += 1,
            (Direction::Down, Direction::Left) => winding_number += 1,
            (Direction::Left, Direction::Up) => winding_number += 1,
            (Direction::Up, Direction::Left) => winding_number -= 1,
            (Direction::Left, Direction::Down) => winding_number -= 1,
            (Direction::Down, Direction::Right) => winding_number -= 1,
            (Direction::Right, Direction::Up) => winding_number -= 1,
            _ => {}
        }

        prev_dir = dir;
    }

    let mut inside: Vec<bool> = vec![false; (map.width * map.height) as usize];

    for i in path.windows(2) {
        let from = i[0];
        let to = i[1];
        let dir = match (to.0 - from.0, to.1 - from.1) {
            (0, -1) => Direction::Up,
            (0, 1) => Direction::Down,
            (-1, 0) => Direction::Left,
            (1, 0) => Direction::Right,
            _ => panic!("invalid delta"),
        };

        let offset = mult(winding_number.signum(), match dir {
            Direction::Up => (1,0),
            Direction::Down => (-1,0),
            Direction::Left => (0,-1),
            Direction::Right => (0,1),
            _ => panic!()
        });

        let neighbor0 = add(from, offset);
        let neighbor1 = add(to, offset);

        flood_fill(&map, neighbor0, &visited, &mut inside);
        flood_fill(&map, neighbor1, &visited, &mut inside);
    }

    let count = inside.iter().filter(|&v| *v).count();
    

    // for y in 0..map.height {
    //     for x in 0..map.width {
    //         if inside[map.get_index(x, y)] {
    //             print!("#");
    //         } else if visited[map.get_index(x, y)] {
    //             print!(".");
    //         } else {
    //             print!(" ");
    //         }
    //     }
    //     println!();
    // }

    count.to_string()
}

fn add(a: (i64, i64), b: (i64, i64)) -> (i64, i64) {
    (a.0 + b.0, a.1 + b.1)
}
fn mult(a: i64, b: (i64, i64)) -> (i64, i64) {
    (a * b.0, a * b.1)
}

fn flood_fill(map: &Map, start: (i64, i64), visited: &[bool], inside: &mut Vec<bool>) {
    if visited[map.get_index(start.0, start.1)] || inside[map.get_index(start.0, start.1)] {
        return;
    }

    let mut stack: Vec<(i64, i64)> = vec![start];

    while let Some((x, y)) = stack.pop() {
        if inside[map.get_index(x, y)] || visited[map.get_index(x, y)]{
            continue;
        }

        inside[map.get_index(x, y)] = true;

        if x > 0 && !inside[map.get_index(x - 1, y)] && visited[map.get_index(x - 1, y)] {
            stack.push((x - 1, y));
        }
        if x < map.width - 1
            && !inside[map.get_index(x + 1, y)]
            && !visited[map.get_index(x + 1, y)]
        {
            stack.push((x + 1, y));
        }
        if y > 0 && !inside[map.get_index(x, y - 1)] && !visited[map.get_index(x, y - 1)] {
            stack.push((x, y - 1));
        }
        if y < map.height - 1
            && !inside[map.get_index(x, y + 1)]
            && !visited[map.get_index(x, y + 1)]
        {
            stack.push((x, y + 1));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT1: &str = ".....
.S-7.
.|.|.
.L-J.
.....";
    const INPUT2: &str = "..F7.
.FJ|.
SJ.L7
|F--J
LJ...";

    // ..F7.
    // .FJ|.
    // SJ.L7
    // |F--J
    // LJ...

    #[test]
    fn test_part1() {
        let result = part1(INPUT1);
        assert_eq!(result, "4");

        let result = part1(INPUT2);
        assert_eq!(result, "8");
    }

    #[test]
    fn test_part2() {
        let result = part2(INPUT1);
        // assert_eq!(result, "4");
    }
}
