use core::cmp::Reverse;
use regex::Regex;
use std::collections::{HashMap, HashSet};

macro_rules! string_vec {
    ($($x:expr),*) => (vec![$($x.to_string()),*]);
}

macro_rules! hashmap(
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key.to_string(), $value.to_string());
            )+
            m
        }
     };
);

struct Token {
    type_: String,
    value: String,
    is_whitespace: bool,
    start: usize,
    stop: usize,
}

struct BinaryOperatorInfo {
    left_priority: i32,
    right_priority: i32,
}

struct BaseSyntax {
    symbols: Vec<String>,
    lookup: HashMap<String, Vec<String>>,
    binary_operator_info: HashMap<String, BinaryOperatorInfo>,
    primary_binary_operators_lookup: HashSet<String>,
    prefix_operators_lookup: HashSet<String>,
    postfix_operators_lookup: HashSet<String>,
    keyword_values_lookup: HashSet<String>,
    keyword_lookup: HashSet<String>,
    non_standard_keyword_lookup: HashSet<String>,
    hex_map: HashSet<String>,

    symbol_characters: Vec<String>,
    number_annotations: Vec<String>,
    keywords: Vec<String>,
    non_standard_keywords: Vec<String>,
    keyword_values: Vec<String>,
    prefix_operators: Vec<String>,
    postfix_operators: Vec<String>,
    binary_operators: Vec<Vec<String>>,
    primary_binary_operators: Vec<String>,
    binary_operator_function_translate: HashMap<String, String>,
    postfix_operator_function_translate: HashMap<String, String>,
    prefix_operator_function_translate: HashMap<String, String>,
}

impl BaseSyntax {
    fn add_symbols(symbols: &mut Vec<String>, strings: &Vec<String>) {
        let re = Regex::new(r"[^\p{L}\d\s@#]").unwrap();

        for symbol in strings {
            if re.is_match(symbol.as_str()) {
                symbols.push(symbol.to_string());
            }
        }
    }

    fn add_binary_symbols(symbols: &mut Vec<String>, strings: &Vec<Vec<String>>) {
        for string_vec in strings {
            for string in string_vec {
                if string.starts_with("R") {
                    symbols.push(string[1..].to_string());
                } else {
                    symbols.push(string.to_string());
                }
            }
        }
    }

    fn is_letter(c: char) -> bool {
        (c >= 'a' && c <= 'z')
            || (c >= 'A' && c <= 'Z')
            || c == '_'
            || c == '@'
            || c >= 127u8 as char
    }

    fn is_during_letter(c: char) -> bool {
        (c >= 'a' && c <= 'z')
            || (c >= '0' && c <= '9')
            || (c >= 'A' && c <= 'Z')
            || c == '_'
            || c == '@'
            || c >= 127u8 as char
    }

    fn is_number(c: char) -> bool {
        c >= '0' && c <= '9'
    }

    fn is_space(c: char) -> bool {
        c > 0u8 as char && c <= 32u8 as char
    }

    fn is_symbol(c: char) -> bool {
        c != '_'
            && ((c >= '!' && c <= '/')
                || (c >= ':' && c <= '?')
                || (c >= '[' && c <= '`')
                || (c >= '{' && c <= '~'))
    }

    fn is_valid_hex(self, c: char) -> bool {
        self.hex_map.contains(&c.to_string())
    }

    fn is_primary_binary_operator(&self, token: &Token) -> bool {
        self.primary_binary_operators_lookup.contains(&token.value)
    }

    pub fn build(&mut self) {
        {
            Self::add_binary_symbols(&mut self.symbols, &self.binary_operators);
            Self::add_symbols(&mut self.symbols, &self.symbol_characters);
            Self::add_symbols(&mut self.symbols, &self.keywords);
            Self::add_symbols(&mut self.symbols, &self.keyword_values);
            Self::add_symbols(&mut self.symbols, &self.prefix_operators);
            Self::add_symbols(&mut self.symbols, &self.postfix_operators);
            Self::add_symbols(&mut self.symbols, &self.primary_binary_operators);

            self.symbols.sort_by_key(|b| Reverse(b.len()))
        }

        {
            let re = Regex::new(r"(.*)A(.*)B(.*)").unwrap();
            for (key, val) in &self.binary_operator_function_translate {
                let caps = re.captures(&val).unwrap();

                let mut left = caps.get(1).map_or("", |m| m.as_str()).to_string();
                let mid = caps.get(2).map_or("", |m| m.as_str()).to_string();
                let mut right = caps.get(3).map_or("", |m| m.as_str()).to_string();

                self.lookup.insert(
                    key.to_string(),
                    [" ".to_string() + &left, mid, " ".to_string() + &right].to_vec(),
                );
            }
        }

        {
            let re = Regex::new(r"(.*)A(.*)").unwrap();
            for (key, val) in &self.prefix_operator_function_translate {
                let caps = re.captures(&val).unwrap();

                let mut left = caps.get(1).map_or("", |m| m.as_str()).to_string();
                let mut right = caps.get(2).map_or("", |m| m.as_str()).to_string();

                self.lookup.insert(
                    key.to_string(),
                    [" ".to_string() + &left, " ".to_string() + &right].to_vec(),
                );
            }
        }

        {
            let re = Regex::new(r"(.*)A(.*)").unwrap();
            for (key, val) in &self.postfix_operator_function_translate {
                let caps = re.captures(&val).unwrap();

                let mut left = caps.get(1).map_or("", |m| m.as_str()).to_string();
                let mut right = caps.get(2).map_or("", |m| m.as_str()).to_string();

                self.lookup.insert(
                    key.to_string(),
                    [" ".to_string() + &left, " ".to_string() + &right].to_vec(),
                );
            }
        }

        {
            let mut priority = 0;

            for group in &self.binary_operators {
                for token in group {
                    if token.starts_with("R") {
                        self.binary_operator_info.insert(
                            token[1..].to_string(),
                            BinaryOperatorInfo {
                                left_priority: priority + 1,
                                right_priority: priority,
                            },
                        );
                    } else {
                        self.binary_operator_info.insert(
                            token.to_string(),
                            BinaryOperatorInfo {
                                left_priority: priority,
                                right_priority: priority,
                            },
                        );
                    }
                }

                priority = priority + 1;
            }
        }

        for key in &self.primary_binary_operators {
            self.primary_binary_operators_lookup.insert(key.to_string());
        }

        for key in &self.prefix_operators {
            self.prefix_operators_lookup.insert(key.to_string());
        }

        for key in &self.postfix_operators {
            self.postfix_operators_lookup.insert(key.to_string());
        }

        for key in &self.keyword_values {
            self.keyword_values_lookup.insert(key.to_string());
        }

        for key in &self.keyword_values {
            self.keyword_lookup.insert(key.to_string());
        }

        for key in &self.non_standard_keywords {
            self.non_standard_keyword_lookup.insert(key.to_string());
        }

        for key in [
            "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f", "A",
            "B", "C", "D", "E", "F",
        ] {
            self.hex_map.insert(key.to_string());
        }
    }
}

fn lua_syntax() -> BaseSyntax {
    BaseSyntax {
        symbols: string_vec![],
        lookup: HashMap::new(),
        binary_operator_info: HashMap::new(),
        primary_binary_operators_lookup: HashSet::new(),
        prefix_operators_lookup: HashSet::new(),
        postfix_operators_lookup: HashSet::new(),
        keyword_values_lookup: HashSet::new(),
        keyword_lookup: HashSet::new(),
        non_standard_keyword_lookup: HashSet::new(),
        hex_map: HashSet::new(),

        symbol_characters: string_vec![
            ",", ";", "(", ")", "{", "}", "[", "]", "=", "::", '"', "'", "<|", "|>"
        ],
        number_annotations: string_vec!["ull", "ll", "ul", "i"],
        keywords: string_vec![
            "do", "end", "if", "then", "else", "elseif", "for", "in", "while", "repeat", "until",
            "break", "return", "local", "function", "and", "not", "or",
            // these are just to make sure all code is covered by tests
            "ÆØÅ", "ÆØÅÆ"
        ],
        non_standard_keywords: string_vec!["continue", "import", "literal", "mutable"],
        keyword_values: string_vec!["...", "nil", "true", "false"],
        prefix_operators: string_vec!["-", "#", "not", "!", "~", "supertype"],
        postfix_operators: string_vec![
            // these are just to make sure all code is covered by tests
            "++", "ÆØÅ", "ÆØÅÆ"
        ],
        binary_operators: vec![
            string_vec!["or", "||"],
            string_vec!["and", "&&"],
            string_vec!["<", ">", "<=", ">=", "~=", "==", "!="],
            string_vec!["|"],
            string_vec!["~"],
            string_vec!["&"],
            string_vec!["<<", ">>"],
            string_vec!["R.."], // right associative
            string_vec!["+", "-"],
            string_vec!["*", "/", "/idiv/", "%"],
            string_vec!["R^"], // right associative
        ],
        primary_binary_operators: string_vec![".", ":"],
        binary_operator_function_translate: hashmap![
            ">>" => "bit.rshift(A, B)",
            "<<" => "bit.lshift(A, B)",
            "|" => "bit.bor(A, B)",
            "&" => "bit.band(A, B)",
            "//" => "math.floor(A / B)",
            "~" => "bit.bxor(A, B)"
        ],
        prefix_operator_function_translate: hashmap![
            "~" => "bit.bnot(A)"
        ],
        postfix_operator_function_translate: hashmap![
            "++" => "A = A + 1",
            "ÆØÅ" => "(A)",
            "ÆØÅÆ" => "(A)"
        ],
    }
}

fn main() {
    let mut syntax = lua_syntax();
    syntax.build();
    println!("syntax: {}", syntax.symbols[0])
}
