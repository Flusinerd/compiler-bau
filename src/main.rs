mod input;
mod repl;
mod scanner;

fn main() {
    // If a file is passed in, then execute the file, else run the repl
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        let filename = &args[1];
        println!("Running file: {}", filename);
        input::run(filename.to_string());
    } else {
        repl::run();
    }
}
