fn parse_batch(s: &str) -> (u32, u32, u32) {
    let rgb_color_count: (u32, u32, u32) = s.split(", ")
        .map(|s| {
            let v: Vec<&str> = s.split(" ").collect();
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

fn is_batch_valid((r, g, b): (u32, u32, u32)) -> bool {
    let red_limit   = 12;
    let green_limit = 13;
    let blue_limit  = 14;
    r <= red_limit && g <= green_limit && b <= blue_limit
}

fn task_1_fully_functional(data: &String) -> u32 {
    let ret: u32 = data.lines().enumerate()
        .map(|(line_idx, line)| {
            let game_is_valid = line
                .split(": ") // separate game from colors
                .last().unwrap() // take just the colors
                .split("; ") // split in batches
                .map(parse_batch) // extract batch as (r, g, b)
                .all(is_batch_valid); // say if the batch is valid

            if game_is_valid { 
                (line_idx + 1) as u32
            } else {
                0
            }})
        .sum();

    //println!("{ret}");
    ret
}

fn task_1_semi_functional(data: &String) -> u32 {
    let mut ret = 0;

    for (game_idx, line) in data.lines().enumerate() {
        let game_valid = line
            .split(": ") // separate game from colors
            .last().unwrap() // take just the colors
            .split("; ") // split in batches
            .map(parse_batch) // extract batch as (r, g, b)
            .all(is_batch_valid); // say if the batch is valid

        ret += if game_valid { (game_idx + 1) as u32 } else { 0 };
    }

    println!("{ret}");
    ret
}

fn main() {
    if let Ok(data) = std::fs::read_to_string("./input.txt") {
        assert_eq!(
            task_1_fully_functional(&data),
            task_1_semi_functional(&data));
    }
}
