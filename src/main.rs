use rayon::iter::ParallelIterator;
use std::io::{self, BufReader};
use std::process::ExitCode;

use clap::Parser;
use memmap2::MmapOptions;
use rayon::iter::IntoParallelIterator;
use rgrep::nfa::NFA;
use rgrep::regex::{sanitize_regex_pattern, RegexAction, RegexParser};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Use extended regex
    #[arg(short = 'E', action)]
    extended: bool,
    #[arg(short = 'n', action)]
    line_num: bool,
    #[arg(short = 'c', default_value_t = 0)]
    context: usize,
    /// Regex pattern to match
    pattern: String,
    #[arg(trailing_var_arg = true)]
    files: Vec<String>,
}

impl Args {
    fn run(self) -> ExitCode {
        let sanitized_pattern = sanitize_regex_pattern(&self.pattern);
        let mut parser = RegexParser::new(&sanitized_pattern);
        let pattern_ast = match parser.parse_regex() {
            Ok(ast) => ast,
            Err(e) => {
                eprintln!("Error parsing regex: {}", e);
                return ExitCode::FAILURE;
            }
        };

        let nfa = RegexAction::compile(pattern_ast, 0);

        let mut found_match = false;

        if self.files.is_empty() {
            found_match = self.process_input(nfa, io::stdin().lock());
        } else {
            for file_path in &self.files {
                match std::fs::File::open(&file_path) {
                    Ok(file) => {
                        let bufread = BufReader::new(file);
                        if self.process_input(nfa.clone(), bufread) {
                            found_match = true;
                        }
                    }
                    Err(_) => eprintln!("Counld not open file at: {}", &file_path),
                }
            }
        }

        if found_match {
            println!("FOUND");
            ExitCode::SUCCESS
        } else {
            println!("NOT FOUND");
            ExitCode::FAILURE
        }
    }

    fn run_parallel(self) -> ExitCode {
        let sanitized_pattern = sanitize_regex_pattern(&self.pattern);
        let mut parser = RegexParser::new(&sanitized_pattern);
        let pattern_ast = match parser.parse_regex() {
            Ok(ast) => ast,
            Err(e) => {
                eprintln!("Error parsing regex: {}", e);
                return ExitCode::FAILURE;
            }
        };

        let nfa = RegexAction::compile(pattern_ast, 0);

        if self.files.is_empty() {
            if self.process_input(nfa, io::stdin().lock()) {
                return ExitCode::SUCCESS;
            } else {
                return ExitCode::FAILURE;
            }
        } else {
            for file_path in &self.files {
                match std::fs::File::open(&file_path) {
                    Ok(file) => {
                        let map = unsafe {
                            MmapOptions::new()
                                .map(&file)
                                .expect("Failed to load the file into a map")
                        };

                        let mut boundaries = vec![0];
                        const CHUNK_SIZE: usize = 500_000;
                        let mut i = 0;
                        while i < map.len() {
                            i += CHUNK_SIZE;
                            if i >= map.len() {
                                break;
                            }

                            while map[i] != b'\n' {
                                // walk till new line
                                i += 1;
                            }

                            if i < map.len() {
                                boundaries.push(i + 1);
                            }
                        }

                        boundaries.push(map.len());

                        let results: Vec<bool> = (0..boundaries.len() - 1)
                            .into_par_iter()
                            .map(|i| {
                                let start = boundaries[i];
                                let end = boundaries[i + 1];

                                let chunk = &map[start..end];
                                let bufr = BufReader::new(chunk);
                                self.process_input(nfa.clone(), bufr)
                            })
                            .collect();
                        if results.iter().any(|b| *b == true) {
                            return ExitCode::SUCCESS;
                        } else {
                            return ExitCode::FAILURE;
                        }
                    }
                    Err(_) => eprintln!("Counld not open file at: {}", &file_path),
                }
            }
            ExitCode::FAILURE
        }
    }

    fn process_input<R: std::io::BufRead>(&self, compiled_nfa: NFA, reader: R) -> bool {
        let mut matched = false;
        let mut line_buffer: Vec<(usize, String)> = Vec::new();
        let context = self.context;

        for (line_num, line_result) in reader.lines().enumerate() {
            match line_result {
                Ok(line) => line_buffer.push((line_num, line)),
                Err(e) => {
                    eprintln!("Failed to read from buf: {}", e);
                    continue;
                }
            }
        }

        let mut i = 0;
        while i < line_buffer.len() {
            let (num, line) = &line_buffer[i];

            if compiled_nfa.matches(line) {
                matched = true;

                let start = if *num >= context { num - context } else { 0 };
                let end = std::cmp::min(line_buffer.len(), num + context + 1);

                for j in start..end {
                    let (ctx_num, ctx_line) = &line_buffer[j];

                    let prefix = if *ctx_num == *num { ">" } else { " " };

                    if self.line_num {
                        println!("{} #{}: {}", prefix, ctx_num, ctx_line);
                    } else {
                        println!("{} {}", prefix, ctx_line);
                    }
                }

                i = end;

                if i < line_buffer.len() {
                    println!("--");
                }

                continue;
            }

            i += 1;
        }

        matched
    }

    fn process_input_parallel<R: std::io::BufRead>(&self, compiled_nfa: NFA, reader: R) -> bool {
        let mut matched = false;
        let mut line_buffer: Vec<(usize, String)> = Vec::new();
        let context = self.context;

        for (line_num, line_result) in reader.lines().enumerate() {
            match line_result {
                Ok(line) => line_buffer.push((line_num, line)),
                Err(e) => {
                    eprintln!("Failed to read from buf: {}", e);
                    continue;
                }
            }
        }

        let mut i = 0;
        while i < line_buffer.len() {
            let (num, line) = &line_buffer[i];

            if compiled_nfa.matches(line) {
                matched = true;

                let start = if *num >= context { num - context } else { 0 };
                let end = std::cmp::min(line_buffer.len(), num + context + 1);

                for j in start..end {
                    let (ctx_num, ctx_line) = &line_buffer[j];

                    let prefix = if *ctx_num == *num { ">" } else { " " };

                    if self.line_num {
                        println!("{} #{}: {}", prefix, ctx_num, ctx_line);
                    } else {
                        println!("{} {}", prefix, ctx_line);
                    }
                }

                i = end;

                if i < line_buffer.len() {
                    println!("--");
                }

                continue;
            }

            i += 1;
        }

        matched
    }
}

fn main() -> ExitCode {
    Args::parse().run_parallel()
}
