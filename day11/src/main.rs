fn part1(input: &str) {
    // Get the width and height of the input grid
    let width = input.lines().next().unwrap().len();
    let height = input.lines().count();
    let map: Vec<char> = input.chars().filter(|c| *c != '\n' && *c != '\r').collect();

    let mut rows: Vec<usize> = Vec::new();
    let mut columns: Vec<usize> = Vec::new();

    for (i, line) in input.lines().enumerate() {
        if line.chars().all(|c| c == '.') {
            rows.push(i);
        }
    }
    for x in 0..width {
        let mut all_dots = true;
        for y in 0..height {
            let index = y * width + x;
            if map[index] != '.' {
                all_dots = false;
                break;
            }
        }
        if all_dots {
            columns.push(x);
        }
    }

    let mut new_map: Vec<char> = Vec::new();

    let new_width = width + columns.len();
    let new_height = height + rows.len();

    for (i, line) in input.lines().enumerate() {
        if rows.contains(&i) {
            for _ in 0..new_width {
                new_map.push('.');
            }
        }
        for (j, c) in line.chars().enumerate() {
            if columns.contains(&j) {
                new_map.push('.');
                new_map.push('.');
            } else {
                new_map.push(c);
            }
        }
    }

    let mut star_positions = Vec::new();

    for y in 0..new_height {
        for x in 0..new_width {
            let index = y * new_width + x;
            if new_map[index] == '#' {
                star_positions.push((x as i64, y as i64));
            }
        }
    }

    let mut total_distance = 0;
    for (i, star1) in star_positions.iter().enumerate() {
        for star2 in star_positions.iter().skip(i + 1) {
            let distance = get_distance(star1, star2);
            total_distance += distance;
        }
    }
    print!("Total distance: {}", total_distance);
}

fn get_distance(star1: &(i64, i64), star2: &(i64, i64)) -> i64 {
    let (x1, y1) = star1;
    let (x2, y2) = star2;
    (x2 - x1).abs() + (y2 - y1).abs()
}

fn part2(input: &str) {
    let width = input.lines().next().unwrap().len();
    let height = input.lines().count();
    let map: Vec<char> = input.chars().filter(|c| *c != '\n' && *c != '\r').collect();

    let mut rows = Vec::new();
    let mut columns = Vec::new();

    for (i, line) in input.lines().enumerate() {
        if line.chars().all(|c| c == '.') {
            rows.push(i as i64);
        }
    }
    for x in 0..width {
        let mut all_dots = true;
        for y in 0..height {
            let index = y * width + x;
            if map[index] != '.' {
                all_dots = false;
                break;
            }
        }
        if all_dots {
            columns.push(x as i64);
        }
    }

    let mut star_positions = Vec::new();

    for y in 0..height {
        for x in 0..width {
            let index = y * width + x;
            if map[index] == '#' {
                star_positions.push((x as i64, y as i64));
            }
        }
    }

    let mut total_distance = 0;
    for (i, (x1, y1)) in star_positions.iter().enumerate() {
        for (j ,(x2, y2)) in star_positions.iter().enumerate().skip(i+1) {
            let min_x = *x1.min(x2);
            let max_x = *x1.max(x2);

            let min_y = *y1.min(y2);
            let max_y = *y1.max(y2);

            let mult = 1000000;

            let extra_x = (mult-1) * columns.iter().filter(|&&x| x > min_x && x < max_x).count() as i64;
            let extra_y = (mult-1) * rows.iter().filter(|&&y| y > min_y && y < max_y).count() as i64;



            let distance = get_distance(&(*x1, *y1), &(*x2, *y2)) + extra_x + extra_y;

            total_distance += distance;
        }

    println!("Total distance: {}", total_distance);
}

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    // part1(&input);
    part2(&input);
}
