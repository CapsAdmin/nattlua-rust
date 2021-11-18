#![allow(dead_code)]
use core::cmp::Reverse;
use regex::Regex;
use std::collections::{HashMap, HashSet};

use crate::token::Token;

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

    pub fn add_prefix_operators(&mut self, operators: Vec<String>) {
        self.prefix_operators_lookup.extend(operators.clone());
        Self::add_symbols(&mut self.symbols, &operators);
    }

    pub fn add_postfix_operators(&mut self, operators: Vec<String>) {
        self.postfix_operators_lookup.extend(operators.clone());
        Self::add_symbols(&mut self.symbols, &operators);
    }

    pub fn add_primary_binary_operators(&mut self, operators: Vec<String>) {
        self.primary_binary_operators_lookup.extend(operators.clone());
        Self::add_symbols(&mut self.symbols, &operators);
    }

    pub fn add_binary_operators(&mut self, operators: Vec<Vec<String>>) {
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
    pub fn add_keywords(&mut self, keywords: Vec<String>) {
        self.keyword_lookup.extend(keywords.clone());
        Self::add_symbols(&mut self.symbols, &keywords);
    }

    pub fn add_keyword_values(&mut self, keyword_values: Vec<String>) {
        self.keyword_values_lookup.extend(keyword_values.clone());
        Self::add_symbols(&mut self.symbols, &keyword_values);
    }

    pub fn add_non_standard_keywords(&mut self, non_standard_keywords: Vec<String>) {
        self.non_standard_keyword_lookup.extend(non_standard_keywords.clone());
        Self::add_symbols(&mut self.symbols, &non_standard_keywords);
    }

    pub fn add_symbol_characters(&mut self, symbol_characters: Vec<String>) {
        Self::add_symbols(&mut self.symbols, &symbol_characters);
    }

    pub fn add_binary_operator_translation(&mut self, operator_translation: HashMap<String, String>) {
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

    pub fn add_prefix_operator_translation(&mut self, operator_translation: HashMap<String, String>) {
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

    pub fn add_postfix_operator_translation(&mut self, operator_translation: HashMap<String, String>) {
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

    pub fn add_number_annotations(&mut self, number_annotations: Vec<String>) {
        self.number_annotations.extend(number_annotations);
    }

    pub fn add_hex_symbols(&mut self, hex_symbols: Vec<String>) {
        self.hex_map.extend(hex_symbols);
    }
}

impl Default for Syntax {
    fn default() -> Self {
        Self {
            symbols: Vec::new(),
            number_annotations: Vec::new(),
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
