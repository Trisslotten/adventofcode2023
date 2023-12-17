fn calculate_load(grid: &Vec<char>, width: usize, height: usize) -> usize {
    let mut load = 0;
    for y in 0..height {
        for x in 0..width {
            let index = y * width + x;
            let c = grid[index];
            if c == 'O' {
                load += height - y;
            }
        }
    }
    load
}

fn tilt_north(grid: &mut Vec<char>, width: usize, height: usize) {
    for y in 0..height {
        for x in 0..width {
            let index = y * width + x;
            let c = grid[index];
            if c == 'O' {
                let mut last_y2 = y;
                for y2 in (0..y).rev() {
                    let c2 = grid[y2 * width + x];
                    if c2 != '.' {
                        break;
                    }
                    last_y2 = y2;
                }
                let new_index = last_y2 * width + x;
                grid[index] = '.';
                grid[new_index] = 'O';
            }
        }
    }
}
fn tilt_south(grid: &mut Vec<char>, width: usize, height: usize) {
    for y in (0..height).rev() {
        for x in 0..width {
            let index = y * width + x;
            if grid[index] == 'O' {
                let mut last_y2 = y;
                for y2 in (y + 1)..height {
                    if grid[y2 * width + x] != '.' {
                        break;
                    }
                    last_y2 = y2;
                }
                let new_index = last_y2 * width + x;
                grid[index] = '.';
                grid[new_index] = 'O';
            }
        }
    }
}
fn tilt_west(grid: &mut Vec<char>, width: usize, height: usize) {
    for y in 0..height {
        for x in 0..width {
            let index = y * width + x;
            if grid[index] == 'O' {
                let mut last_x2 = x;
                for x2 in (0..x).rev() {
                    if grid[y * width + x2] != '.' {
                        break;
                    }
                    last_x2 = x2;
                }
                let new_index = y * width + last_x2;
                grid[index] = '.';
                grid[new_index] = 'O';
            }
        }
    }
}
fn tilt_east(grid: &mut Vec<char>, width: usize, height: usize) {
    for y in 0..height {
        for x in (0..width).rev() {
            let index = y * width + x;
            if grid[index] == 'O' {
                let mut last_x2 = x;
                for x2 in (x + 1)..width {
                    if grid[y * width + x2] != '.' {
                        break;
                    }
                    last_x2 = x2;
                }
                let new_index = y * width + last_x2;
                grid[index] = '.';
                grid[new_index] = 'O';
            }
        }
    }
}

fn parse_grid(input: &str) -> (usize, usize, Vec<char>) {
    let height = input.lines().count();
    let width = input.lines().next().unwrap().len();
    let mut grid: Vec<char> = Vec::new();
    grid.reserve(width * height);
    for line in input.lines() {
        for c in line.chars() {
            grid.push(c);
        }
    }
    (height, width, grid)
}

fn part1(input: &str) {
    let (height, width, mut grid) = parse_grid(input);

    tilt_north(&mut grid, width, height);
    let load = calculate_load(&grid, width, height);
    println!("load: {}", load);
}

fn print_grid(grid: &Vec<char>, width: usize, height: usize) {
    for y in 0..height {
        for x in 0..width {
            let index = y * width + x;
            print!("{}", grid[index]);
        }
        println!();
    }
    println!();
}

fn part2(input: &str) {
    let (height, width, mut grid) = parse_grid(input);

    for i in 0..10000 {
        do_cycle(&mut grid, width, height);
        let load = calculate_load(&grid, width, height);
        println!("{i} -> load: {}", load);
    }

    /*
    9949 -> load: 90968
    9950 -> load: 90940
    9951 -> load: 90906
    9952 -> load: 90850
    9953 -> load: 90780
    9954 -> load: 90713
    9955 -> load: 90653
    9956 -> load: 90592
    9957 -> load: 90552
    9958 -> load: 90540
    9959 -> load: 90521
    9960 -> load: 90514
    9961 -> load: 90512
    9962 -> load: 90539
    9963 -> load: 90575
    9964 -> load: 90618
    9965 -> load: 90671
    9966 -> load: 90733
    9967 -> load: 90802
    9968 -> load: 90871
    9969 -> load: 90928
    9970 -> load: 90975
    9971 -> load: 90997
    9972 -> load: 90991
    9973 -> load: 90998
    9974 -> load: 90987
    9975 -> load: 90968
    9976 -> load: 90940
    9977 -> load: 90906
    9978 -> load: 90850
    9979 -> load: 90780
    9980 -> load: 90713
    9981 -> load: 90653
    9982 -> load: 90592
    9983 -> load: 90552
    9984 -> load: 90540
    9985 -> load: 90521
    9986 -> load: 90514
    9987 -> load: 90512
    9988 -> load: 90539
    9989 -> load: 90575
    9990 -> load: 90618
    9991 -> load: 90671
    9992 -> load: 90733
    9993 -> load: 90802
    9994 -> load: 90871
    9995 -> load: 90928 <---
    9996 -> load: 90975
    9997 -> load: 90997
    9998 -> load: 90991
    9999 -> load: 90998
    10000 -> load: 90987
    52 length cycle
    */
}

fn do_cycle(grid: &mut Vec<char>, width: usize, height: usize) {
    tilt_north(grid, width, height);
    tilt_west(grid, width, height);
    tilt_south(grid, width, height);
    tilt_east(grid, width, height);
}

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    part1(&input);
    part2(&input);
}
