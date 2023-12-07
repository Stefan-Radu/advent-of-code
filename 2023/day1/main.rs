fn process_string(s: &str) -> u32 {
    //let chars: Vec<i32> = s
        //.chars()
        //.map(|x: char| x.to_digit(10))
        //.filter(|opt| x.is_some())
        //.map(|opt| opt.unwrap())
        //.collect();
    //match chars.first() {
        //Some(elem) => {
            //(elem - '0' as i32) * 10 
                //+ (chars.last().unwrap()) - '0' as i32
        //},
        //None => 0
    //}

    let chars: Vec<_> = s
        .chars()
        .filter_map(|x: char| x.to_digit(10))
        .collect();

    // this can panic!
    //let c = chars.get(0); -> gives option
    chars[0] * 10 + chars[chars.len() - 1]
}

fn task_1(c: &String) {
    let res: u32 = c.lines()
        .map(process_string)
        .sum();
    println!("{}", res);
}

fn task_2(c: &String) {
    let numbers: Vec<&str> = vec![
        "zero", "one", "two", "three", "four",
        "five", "six", "seven", "eight", "nine"];

    let mut data: String = c.clone();

    for (nr, s) in numbers.iter().enumerate() {
        let mut cnt = 1;
        for (idx, _) in data.clone().match_indices(s) {
            let ch: char = char::from_digit(nr as u32, 10).unwrap();
            data.insert(idx + cnt, ch);
            cnt += 1;
        }
    }

    task_1(&data);
}

fn main() {
    let input_file_path = "./input.txt";

    match std::fs::read_to_string(input_file_path) {
        Ok(content) => {
            task_1(&content);
            task_2(&content);
        }
        Err(e) => {
            eprintln!("{e}");
        }
    }
}
