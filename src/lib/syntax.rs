#![allow(dead_code)]
use core::cmp::Reverse;
use regex::Regex;
use std::collections::{HashMap, HashSet};

use crate::token::Token;

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

struct BinaryOperatorInfo {
    left_priority: usize,
    right_priority: usize,
}

pub struct Syntax {
    symbols: Vec<String>,
    number_annotations: Vec<String>,
    lookup: HashMap<String, Vec<String>>,
    binary_operator_info: HashMap<String, BinaryOperatorInfo>,
    primary_binary_operators_lookup: HashSet<String>,
    prefix_operators_lookup: HashSet<String>,
    postfix_operators_lookup: HashSet<String>,
    keyword_values_lookup: HashSet<String>,
    keyword_lookup: HashSet<String>,
    non_standard_keyword_lookup: HashSet<String>,
    hex_map: HashSet<String>,
}

impl Syntax {
    fn add_symbols(symbols: &mut Vec<String>, strings: &[String]) {
        let re = Regex::new(r"[^\p{L}\d\s@#]").unwrap();

        for symbol in strings {
            if re.is_match(symbol.as_str()) {
                symbols.push(symbol.to_string());
            }
        }

        symbols.sort_by_key(|b| Reverse(b.len()))
    }

    pub fn symbols(&self) -> Vec<String> {
        self.symbols.clone()
    }

    pub fn number_annotations(&self) -> Vec<String> {
        self.number_annotations.clone()
    }

    fn add_binary_symbols(symbols: &mut Vec<String>, strings: &[Vec<String>]) {
        for string_vec in strings {
            for string in string_vec {
                if let Some(stripped) = string.strip_prefix('R') {
                    symbols.push(stripped.to_string());
                } else {
                    symbols.push(string.to_string());
                }
            }
        }
    }

    pub fn is_letter(c: char) -> bool {
        ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_' || c == '@' || c >= 127u8 as char
    }

    pub fn is_during_letter(c: char) -> bool {
        ('a'..='z').contains(&c)
            || ('0'..='9').contains(&c)
            || ('A'..='Z').contains(&c)
            || c == '_'
            || c == '@'
            || c >= 127u8 as char
    }

    pub fn is_number(c: char) -> bool {
        ('0'..='9').contains(&c)
    }

    pub fn is_space(c: char) -> bool {
        c > 0u8 as char && c <= 32u8 as char
    }

    pub fn is_symbol(c: char) -> bool {
        c != '_'
            && (('!'..='/').contains(&c)
                || (':'..='?').contains(&c)
                || ('['..='`').contains(&c)
                || ('{'..='~').contains(&c))
    }

    pub fn is_valid_hex(&self, c: char) -> bool {
        self.hex_map.contains(&c.to_string())
    }

    fn is_primary_binary_operator(&self, token: &Token) -> bool {
        self.primary_binary_operators_lookup.contains(&token.value)
    }

    fn is_prefix_operator(&self, token: &Token) -> bool {
        self.prefix_operators_lookup.contains(&token.value)
    }

    fn is_postfix_operator(&self, token: &Token) -> bool {
        self.postfix_operators_lookup.contains(&token.value)
    }

    fn is_keyword(&self, token: &Token) -> bool {
        self.keyword_lookup.contains(&token.value)
    }

    fn is_non_standard_keyword(&self, token: &Token) -> bool {
        self.non_standard_keyword_lookup.contains(&token.value)
    }

    fn is_keyword_value(&self, token: &Token) -> bool {
        self.keyword_values_lookup.contains(&token.value)
    }

    fn get_operator_info(&self, token: &Token) -> &BinaryOperatorInfo {
        self.binary_operator_info.get(&token.value).unwrap()
    }

    fn add_prefix_operators(&mut self, operators: Vec<String>) {
        self.prefix_operators_lookup.extend(operators.clone());
        Self::add_symbols(&mut self.symbols, &operators);
    }

    fn add_postfix_operators(&mut self, operators: Vec<String>) {
        self.postfix_operators_lookup.extend(operators.clone());
        Self::add_symbols(&mut self.symbols, &operators);
    }

    fn add_primary_binary_operators(&mut self, operators: Vec<String>) {
        self.primary_binary_operators_lookup.extend(operators.clone());
        Self::add_symbols(&mut self.symbols, &operators);
    }

    fn add_binary_operators(&mut self, operators: Vec<Vec<String>>) {
        Self::add_binary_symbols(&mut self.symbols, &operators);
        {
            for (priority, group) in operators.iter().enumerate() {
                for token in group {
                    if let Some(stripped) = token.strip_prefix('R') {
                        self.binary_operator_info.insert(
                            stripped.to_string(),
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
            }
        }
    }
    fn add_keywords(&mut self, keywords: Vec<String>) {
        self.keyword_lookup.extend(keywords.clone());
        Self::add_symbols(&mut self.symbols, &keywords);
    }

    fn add_keyword_values(&mut self, keyword_values: Vec<String>) {
        self.keyword_values_lookup.extend(keyword_values.clone());
        Self::add_symbols(&mut self.symbols, &keyword_values);
    }

    fn add_non_standard_keywords(&mut self, non_standard_keywords: Vec<String>) {
        self.non_standard_keyword_lookup.extend(non_standard_keywords.clone());
        Self::add_symbols(&mut self.symbols, &non_standard_keywords);
    }

    fn add_symbol_characters(&mut self, symbol_characters: Vec<String>) {
        Self::add_symbols(&mut self.symbols, &symbol_characters);
    }

    fn add_binary_operator_translation(&mut self, operator_translation: HashMap<String, String>) {
        let re = Regex::new(r"(.*)A(.*)B(.*)").unwrap();
        for (key, val) in &operator_translation {
            let caps = re.captures(val).unwrap();

            let left = caps.get(1).map_or("", |m| m.as_str()).to_string();
            let mid = caps.get(2).map_or("", |m| m.as_str()).to_string();
            let right = caps.get(3).map_or("", |m| m.as_str()).to_string();

            self.lookup.insert(
                key.to_string(),
                [" ".to_string() + &left, mid, " ".to_string() + &right].to_vec(),
            );
        }
    }

    fn add_prefix_operator_translation(&mut self, operator_translation: HashMap<String, String>) {
        let re = Regex::new(r"(.*)A(.*)").unwrap();
        for (key, val) in &operator_translation {
            let caps = re.captures(val).unwrap();

            let left = caps.get(1).map_or("", |m| m.as_str()).to_string();
            let right = caps.get(2).map_or("", |m| m.as_str()).to_string();

            self.lookup.insert(
                key.to_string(),
                [" ".to_string() + &left, " ".to_string() + &right].to_vec(),
            );
        }
    }

    fn add_postfix_operator_translation(&mut self, operator_translation: HashMap<String, String>) {
        let re = Regex::new(r"(.*)A(.*)").unwrap();
        for (key, val) in &operator_translation {
            let caps = re.captures(val).unwrap();

            let left = caps.get(1).map_or("", |m| m.as_str()).to_string();
            let right = caps.get(2).map_or("", |m| m.as_str()).to_string();

            self.lookup.insert(
                key.to_string(),
                [" ".to_string() + &left, " ".to_string() + &right].to_vec(),
            );
        }
    }

    fn add_number_annotations(&mut self, number_annotations: Vec<String>) {
        self.number_annotations.extend(number_annotations);
    }

    fn add_hex_symbols(&mut self, hex_symbols: Vec<String>) {
        self.hex_map.extend(hex_symbols);
    }
}

impl Default for Syntax {
    fn default() -> Self {
        Self {
            symbols: string_vec![],
            number_annotations: string_vec![],
            lookup: HashMap::new(),
            binary_operator_info: HashMap::new(),
            primary_binary_operators_lookup: HashSet::new(),
            prefix_operators_lookup: HashSet::new(),
            postfix_operators_lookup: HashSet::new(),
            keyword_values_lookup: HashSet::new(),
            keyword_lookup: HashSet::new(),
            non_standard_keyword_lookup: HashSet::new(),
            hex_map: HashSet::new(),
        }
    }
}

pub fn lua_syntax() -> Syntax {
    let mut syntax = Syntax::default();
    syntax.add_number_annotations(string_vec!["ull", "ll", "ul", "i"]);
    syntax.add_prefix_operator_translation(hashmap![
        "~" => "bit.bnot(A)"
    ]);
    syntax.add_postfix_operator_translation(hashmap![
        "++" => "A = A + 1",
        "ÆØÅ" => "(A)",
        "ÆØÅÆ" => "(A)"
    ]);
    syntax.add_binary_operator_translation(hashmap![
        ">>" => "bit.rshift(A, B)",
        "<<" => "bit.lshift(A, B)",
        "|" => "bit.bor(A, B)",
        "&" => "bit.band(A, B)",
        "//" => "math.floor(A / B)",
        "~" => "bit.bxor(A, B)"
    ]);
    syntax.add_symbol_characters(string_vec![
        ",", ";", "(", ")", "{", "}", "[", "]", "=", "::", '"', "'", "<|", "|>"
    ]);
    syntax.add_non_standard_keywords(string_vec!["continue", "import", "literal", "mutable"]);
    syntax.add_keyword_values(string_vec!["...", "nil", "true", "false"]);
    syntax.add_keywords(string_vec![
        "do", "end", "if", "then", "else", "elseif", "for", "in", "while", "repeat", "until", "break", "return",
        "local", "function", "and", "not", "or", // these are just to make sure all code is covered by tests
        "ÆØÅ", "ÆØÅÆ"
    ]);
    syntax.add_prefix_operators(string_vec!["-", "#", "not", "!", "~", "supertype"]);
    syntax.add_primary_binary_operators(string_vec![".", ":"]);
    syntax.add_postfix_operators(string_vec![
        // these are just to make sure all code is covered by tests
        "++", "ÆØÅ", "ÆØÅÆ"
    ]);
    syntax.add_binary_operators(vec![
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
    ]);
    syntax.add_hex_symbols(string_vec![
        "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f", "A", "B", "C", "D", "E", "F"
    ]);
    syntax
}

pub fn lua_typesystem_syntax() -> Syntax {
    let mut syntax = lua_syntax();

    syntax.add_prefix_operators(string_vec![
        "-",
        "#",
        "not",
        "~",
        "typeof",
        "$",
        "unique",
        "mutable",
        "literal",
        "supertype",
        "expand"
    ]);

    syntax.add_primary_binary_operators(string_vec![".", ":"]);

    syntax.add_binary_operators(vec![
        string_vec!["or"],
        string_vec!["and"],
        string_vec!["extends"],
        string_vec!["subsetof"],
        string_vec!["supersetof"],
        string_vec!["<", ">", "<=", ">=", "~=", "=="],
        string_vec!["|"],
        string_vec!["~"],
        string_vec!["&"],
        string_vec!["<<", ">>"],
        string_vec!["R.."],
        string_vec!["+", "-"],
        string_vec!["*", "/", "/idiv/", "%"],
        string_vec!["R^"],
    ]);

    syntax
}
