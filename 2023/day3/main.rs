fn valid_char_task_1(c: &char) -> bool {
    "*#%@-&+=$/".contains(*c)
}


fn task_1(data: &str) {
    const DIR_LEN: usize = 8;

    let mat: Vec<Vec<char>> = data.lines()
        .map(|s| s.chars().collect())
        .collect();
    let line_len: usize = mat[0].len();

    let dir_i: [i32; DIR_LEN] = [0, 1, 1, 1, 0, -1, -1, -1];
    let dir_j: [i32; DIR_LEN] = [1, 1, 0, -1, -1, -1, 0, 1];

    let symbol_locations: Vec<_> = data.char_indices()
        .filter(|(_idx, c)| valid_char_task_1(c))
        // must add 1 because of line endings which make lines 1 longer actually 
        .map(|(idx, c)| ((idx / (line_len + 1)), (idx % (line_len + 1)), c))
        .collect();

    let mut nr_positions = std::collections::HashSet::<(usize, usize)>::new();

    let mut sum = 0;
    for (i, j, _c) in symbol_locations {
        for dir_idx in 0..DIR_LEN {
            let neighbor = (i as i32 + dir_i[dir_idx], j as i32 + dir_j[dir_idx]);
            if neighbor.0 >= 0 && neighbor.0 < line_len as i32
                && neighbor.1 >= 0 && neighbor.1 < line_len as i32
                && mat[neighbor.0 as usize][neighbor.1 as usize].is_digit(10) {
                // found digit next to symbol
                let nr_line = neighbor.0 as usize;
                let mut nr_idx = neighbor.1 as i32;
                while nr_idx - 1 >= 0 
                    && mat[nr_line][(nr_idx - 1) as usize].is_digit(10) {
                    nr_idx -= 1;
                }

                if nr_positions.insert((nr_line, nr_idx as usize)) {
                    // inserted
                    let mut nr = 0;
                    let mut nr_idx = nr_idx as usize;
                    while nr_idx < line_len && mat[nr_line][nr_idx].is_digit(10) {
                        nr = nr * 10 + mat[nr_line][nr_idx].to_digit(10).unwrap();
                        nr_idx += 1;
                    }

                    sum += nr;
                }
            }
        }
    }

    println!("{sum}");
}

fn task_2(data: &str) {
    /* this is basically task 1 but a little bit modified */
    const DIR_LEN: usize = 8;

    let mat: Vec<Vec<char>> = data.lines()
        .map(|s| s.chars().collect())
        .collect();
    let line_len: usize = mat[0].len();

    let dir_i: [i32; DIR_LEN] = [0, 1, 1, 1, 0, -1, -1, -1];
    let dir_j: [i32; DIR_LEN] = [1, 1, 0, -1, -1, -1, 0, 1];

    let symbol_locations: Vec<_> = data.char_indices()
        .filter(|(_idx, c)| c == &'*')
        // must add 1 because of line endings which make lines 1 longer actually 
        .map(|(idx, c)| ((idx / (line_len + 1)), (idx % (line_len + 1)), c))
        .collect();

    let mut sum = 0;
    for (i, j, _c) in symbol_locations {
        // for each start find all neighboring numbers

        let mut nr_positions = std::collections::HashSet::<(usize, usize)>::new();
        for dir_idx in 0..DIR_LEN {
            let neighbor = (i as i32 + dir_i[dir_idx], j as i32 + dir_j[dir_idx]);
            if neighbor.0 >= 0 && neighbor.0 < line_len as i32
                && neighbor.1 >= 0 && neighbor.1 < line_len as i32
                && mat[neighbor.0 as usize][neighbor.1 as usize].is_digit(10) {
                // found digit next to symbol
                let nr_line = neighbor.0 as usize;
                let mut nr_idx = neighbor.1 as i32;
                while nr_idx - 1 >= 0 
                    && mat[nr_line][(nr_idx - 1) as usize].is_digit(10) {
                    nr_idx -= 1;
                }

                nr_positions.insert((nr_line, nr_idx as usize));
            }
        }

        if nr_positions.len() == 2 {
            let mut p = 1;
            for (ii, mut jj) in nr_positions {
                // inserted
                let mut nr = 0;
                while jj < line_len && mat[ii][jj].is_digit(10) {
                    nr = nr * 10 + mat[ii][jj].to_digit(10).unwrap();
                    jj += 1;
                }

                p *= nr;
            }

            sum += p;
        }
    }

    println!("{sum}");
}

fn main() {
    if let Ok(data) = std::fs::read_to_string("./input.txt") {
        task_1(&data);
        task_2(&data);
    }
}
