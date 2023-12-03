use std::io::Error;

fn process_string(s: &str) -> i32 {
    let chars: Vec<i32> = s
        .chars()
        .filter(|&x| x.is_digit(10))
        .map(|x: char| x as i32)
        .collect();
    match chars.first() {
        Some(elem) => {
            (elem - '0' as i32) * 10 
                + (chars.last().unwrap()) - '0' as i32
        },
        None => 0
    }
}

fn task_1(content: &Result<String, Error>) {
    match content {
        Ok(c) => {
            let res: i32 = c.split('\n')
                .map(|x| process_string(&x.to_string()))
                .sum();
            println!("{}", res);
        }, 
        Err(e) => {
            eprintln!("{e}");
        }
    }
}

fn task_2(content: &Result<String, Error>) {
    let numbers: Vec<&str> = vec![
        "zero", "one", "two", "three", "four",
        "five", "six", "seven", "eight", "nine"];

    match content {
        Ok(c) => {
            let mut data: String = c.clone();

            for (nr, s) in numbers.iter().enumerate() {
                let mut cnt = 1;
                for (idx, _) in data.clone().match_indices(s) {
                    let ch: char = char::from_digit(nr as u32, 10).unwrap();
                    data.insert(idx + cnt, ch);
                    cnt += 1;
                }
            }

            task_1(&Ok(data));
        }, 
        Err(e) => {
            eprintln!("{e}");
        }
    }
}

fn main() {
    let input_file_path = "./input.txt";
    let content = std::fs::read_to_string(input_file_path);

    task_1(&content);
    task_2(&content);
}
