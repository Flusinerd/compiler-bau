use crate::scanner;

pub fn run(filename: String) {
    // Read the contents of the file
    let contents = std::fs::read_to_string(filename).expect("Could not read file");
    let tokens = scanner::scan_tokens(contents).expect("Could not scan tokens");
    // Print all tokens line by line
    for token in tokens {
        println!("{:?}", token);
    }
}
