fn parse_batch(s: &str) -> (u32, u32, u32) {
    // takes a group of extracted cubes
    // returns the rgb count from the group
    let rgb_color_count: (u32, u32, u32) = s.split(", ")
        .map(|s| {
            let v: Vec<&str> = s.split(' ').collect();
            let amount: u32 = v[0].parse().unwrap();
            (amount, v[1])
        })
        .fold((0, 0, 0), |(r, g, b), (a, c)| {
            match c {
                "red"   => (r + a, g, b),
                "green" => (r, g + a, b),
                "blue"  => (r, g, b + a),
                _       => (r, g, b)     
            }
        });

    rgb_color_count 
}

fn is_batch_valid((r, g, b): &(u32, u32, u32)) -> bool {
    // valid if values are leq than limits
    let red_limit   = 12;
    let green_limit = 13;
    let blue_limit  = 14;
    *r <= red_limit && *g <= green_limit && *b <= blue_limit
}

fn extract_rgb(line: &str) -> Vec<(u32, u32, u32)> {
    line.split(": ") // separate game from colors
        .last().unwrap() // take just the colors
        .split("; ") // split in batches
        .map(parse_batch) // extract batch as (r, g, b)
        .collect() // make Vec
}

fn task_1(data: &String) {
    let ret: u32 = data.lines().enumerate()
        .map(|(line_idx, line)| {
            // valid if all batches are valid
            let game_is_valid = extract_rgb(line)
                .iter()
                .all(is_batch_valid);

            if game_is_valid { 
                (line_idx + 1) as u32
            } else {
                0
            }})
        .sum();

    println!("{ret}");
}

fn task_2(data: &String) {
    use std::cmp::max;

    let ret: u32 = data.lines()
        .map(|line| -> u32 {
            let (r_limit, g_limit, b_limit) = extract_rgb(line)
                .iter()
                .fold((0, 0, 0), | acc, &val | ( 
                        max(acc.0, val.0),
                        max(acc.1, val.1),
                        max(acc.2, val.2),
                    ));

            r_limit * g_limit * b_limit
        })
        .sum();

    println!("{ret}");
}

fn main() {
    if let Ok(data) = std::fs::read_to_string("./input.txt") {
        task_1(&data);
        task_2(&data);
    }
}
