mod builtin;
mod interpreter;
mod parser;
use std::io::Read;
fn main() {
    // take the script path as an argument
    let args: Vec<String> = std::env::args().collect();
    let script_path = &args[1];
    // open the file and read the contents to a string
    let mut file = std::fs::File::open(script_path).expect("Failed to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("Failed to read file");
    // parse the script
    let items = parser::parse(contents).expect("Failed to parse script");
    //dbg!(&items);
    let evaled = interpreter::interpret(items);
    println!("{:?}", evaled);
}
