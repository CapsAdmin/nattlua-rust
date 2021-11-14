#![allow(dead_code)]
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

pub enum AnnotationType {
    Hex,
    Decimal,
    Binary,
}

#[derive(Clone)]
pub enum TokenType {
    AnalyzerDebugCode,
    ParserDebugCode,
    Letter,
    String,
    Number,
    Symbol,
    EndOfFile, // whitespace
    Shebang,   // whitespace
    Discard,
    Unknown,

    // whitespace
    LineComment,
    MultilineComment,
    CommentEscape,
    Space,
}

#[derive(Clone)]
struct Token {
    kind: TokenType,
    value: String,
    is_whitespace: bool,
    start: usize,
    stop: usize,
    pub whitespace: Vec<Token>,
}

struct BinaryOperatorInfo {
    left_priority: usize,
    right_priority: usize,
}

struct Syntax {
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

impl Syntax {
    fn add_symbols(symbols: &mut Vec<String>, strings: &[String]) {
        let re = Regex::new(r"[^\p{L}\d\s@#]").unwrap();

        for symbol in strings {
            if re.is_match(symbol.as_str()) {
                symbols.push(symbol.to_string());
            }
        }
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

    fn is_letter(c: char) -> bool {
        ('a'..='z').contains(&c)
            || ('A'..='Z').contains(&c)
            || c == '_'
            || c == '@'
            || c >= 127u8 as char
    }

    fn is_during_letter(c: char) -> bool {
        ('a'..='z').contains(&c)
            || ('0'..='9').contains(&c)
            || ('A'..='Z').contains(&c)
            || c == '_'
            || c == '@'
            || c >= 127u8 as char
    }

    fn is_number(c: char) -> bool {
        ('0'..='9').contains(&c)
    }

    fn is_space(c: char) -> bool {
        c > 0u8 as char && c <= 32u8 as char
    }

    fn is_symbol(c: char) -> bool {
        c != '_'
            && (('!'..='/').contains(&c)
                || (':'..='?').contains(&c)
                || ('['..='`').contains(&c)
                || ('{'..='~').contains(&c))
    }

    fn is_valid_hex(&self, c: char) -> bool {
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

        {
            let re = Regex::new(r"(.*)A(.*)").unwrap();
            for (key, val) in &self.prefix_operator_function_translate {
                let caps = re.captures(val).unwrap();

                let left = caps.get(1).map_or("", |m| m.as_str()).to_string();
                let right = caps.get(2).map_or("", |m| m.as_str()).to_string();

                self.lookup.insert(
                    key.to_string(),
                    [" ".to_string() + &left, " ".to_string() + &right].to_vec(),
                );
            }
        }

        {
            let re = Regex::new(r"(.*)A(.*)").unwrap();
            for (key, val) in &self.postfix_operator_function_translate {
                let caps = re.captures(val).unwrap();

                let left = caps.get(1).map_or("", |m| m.as_str()).to_string();
                let right = caps.get(2).map_or("", |m| m.as_str()).to_string();

                self.lookup.insert(
                    key.to_string(),
                    [" ".to_string() + &left, " ".to_string() + &right].to_vec(),
                );
            }
        }

        {
            for (priority, group) in self.binary_operators.iter().enumerate() {
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

fn lua_syntax() -> Syntax {
    Syntax {
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

fn lua_typesystem_syntax() -> Syntax {
    let mut syntax = lua_syntax();

    syntax.prefix_operators = string_vec![
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
    ];

    syntax.primary_binary_operators = string_vec!["."];

    syntax.binary_operators = vec![
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
    ];

    syntax
}

struct Code {
    buffer: String,
    name: String,
}

impl Code {
    fn new(buffer: String, name: String) -> Code {
        Code { buffer, name }
    }

    fn substring(&self, begin: usize, end: usize) -> String {
        // TODO: string view?

        let start = std::cmp::min(begin, self.get_length());
        let stop = std::cmp::min(end, self.get_length());

        self.buffer[start..stop].to_string()
    }

    fn get_string(&self) -> &String {
        &self.buffer
    }

    fn get_length(&self) -> usize {
        self.buffer.len()
    }

    fn get_byte(&self, pos: usize) -> u8 {
        *self.buffer.as_bytes().get(pos).or(Some(&0u8)).unwrap()
    }

    fn find_nearest(&self, find: &str, from_index: usize) -> Option<usize> {
        self.buffer[from_index..].find(find)
    }
}

struct Lexer {
    code: Code,
    position: usize,
    runtime_syntax: Syntax,
    typesystem_syntax: Syntax,
    comment_escape: bool,
}

impl Lexer {
    fn get_length(&self) -> usize {
        self.code.get_length()
    }

    fn get_string(&self, start: usize, stop: usize) -> String {
        self.code.substring(start, stop)
    }

    fn get_byte_char_offset(&self, offset: usize) -> u8 {
        self.code.get_byte(self.position + offset)
    }

    fn get_current_char(&self) -> char {
        self.get_byte_char_offset(0) as char
    }

    fn set_position(&mut self, position: usize) -> &mut Self {
        self.position = position;
        self
    }

    fn get_position(&self) -> usize {
        self.position
    }

    fn reset_state(&mut self) -> &mut Self {
        self.set_position(0);
        self
    }

    fn find_nearest(&self, str: &str) -> Option<usize> {
        self.code.find_nearest(str, self.get_position())
    }

    fn advance(&mut self, offset: usize) -> &mut Self {
        self.set_position(self.get_position() + offset);
        self
    }

    fn read_char(&mut self) -> char {
        let char = self.get_current_char();
        self.advance(1);
        char
    }
    fn the_end(&self) -> bool {
        self.get_position() >= self.get_length()
    }

    fn is_byte(&self, byte: u8, offset: usize) -> bool {
        self.get_byte_char_offset(offset) == byte
    }

    fn is_current_byte(&self, byte: u8) -> bool {
        self.is_byte(byte, 0)
    }

    fn is_current_value(&self, value: &str) -> bool {
        self.is_value(value, 0)
    }

    fn is_value(&self, value: &str, offset: usize) -> bool {
        self.get_string(
            self.get_position() + offset,
            self.get_position() + offset + value.len(),
        ) == value
    }

    fn error(
        &self,
        message: &str,
        start: Option<usize>,
        stop: Option<usize>,
        args: Option<Vec<String>>,
    ) {
        let mut buffer = String::new();
        buffer.push_str("Error: ");
        buffer.push_str(message);
        buffer.push_str("\n");
        buffer.push_str("    ");
        buffer.push_str(self.code.get_string().as_str());
        buffer.push_str("\n");
        buffer.push_str("    ");
        if let Some(start) = start {
            for _ in 0..start {
                buffer.push_str(" ");
            }
            buffer.push_str("^");
        }
        buffer.push_str("\n");
        if let Some(stop) = stop {
            buffer.push_str("    ");
            for _ in 0..stop {
                buffer.push_str(" ");
            }
            buffer.push_str("^");
        }

        if args.is_some() {
            for arg in args.unwrap() {
                buffer.push_str("\n");
                buffer.push_str("    ");
                buffer.push_str(arg.as_str());
            }
        }
        println!("{}", buffer);
    }

    fn new_token(kind: TokenType, is_whitespace: bool, start: usize, stop: usize) -> Token {
        Token {
            kind,
            is_whitespace,
            start,
            stop,
            value: String::new(),
            whitespace: Vec::new(),
        }
    }

    fn read_from_array(&mut self, array: Vec<String>) -> bool {
        for annotation in array {
            if self
                .get_string(self.get_position(), self.get_position() + annotation.len())
                .to_lowercase()
                == annotation.clone()
            {
                self.advance(annotation.len());
                return true;
            }
        }
        false
    }

    fn read_shebang(&mut self) -> bool {
        if self.get_position() == 0 && self.is_current_value("#") {
            while !self.the_end() {
                self.advance(1);

                if self.is_current_value("\n") {
                    break;
                }
            }
            return true;
        }
        false
    }

    fn read_end_of_file(&mut self) -> bool {
        if self.the_end() {
            self.advance(1);
            return true;
        }
        false
    }

    fn read_unknown(&mut self) -> (Option<TokenType>, bool) {
        self.advance(1);
        (Some(TokenType::Unknown), false)
    }

    fn read(&mut self) -> (Option<TokenType>, bool) {
        {
            let kind = self
                .read_space()
                .or(self.read_comment_escape())
                .or(self.read_multiline_c_comment())
                .or(self.read_line_c_comment())
                .or(self.read_multiline_comment())
                .or(self.read_line_comment());

            if kind.is_some() {
                return (kind, true);
            }
        }

        {
            let kind = self
                .read_analyzer_debug_code()
                .or(self.read_parser_debug_code())
                .or(self.read_number())
                .or(self.read_multiline_string())
                .or(self.read_single_quoted_string())
                .or(self.read_double_quoted_string())
                .or(self.read_letter())
                .or(self.read_symbol());
            if kind.is_some() {
                return (kind, false);
            }
        }

        (None, false)
    }

    fn read_simple(&mut self) -> (TokenType, bool, usize, usize) {
        if self.read_shebang() {
            return (TokenType::Shebang, false, 0, self.get_position());
        }

        let start = self.get_position();

        if let (Some(kind), is_whitespace) = self.read() {
            return (kind, is_whitespace, start, self.get_position());
        }

        if self.read_end_of_file() {
            return (TokenType::EndOfFile, false, start, self.get_position());
        }

        let (kind, is_whitespace) = self.read_unknown();

        assert!(kind.is_some());

        (kind.unwrap(), is_whitespace, start, self.get_position())
    }

    fn read_token(&mut self) -> Token {
        let (token_type, is_whitespace, start, stop) = self.read_simple();
        Self::new_token(token_type, is_whitespace, start, stop)
    }

    fn get_tokens(&mut self) -> Vec<Token> {
        self.reset_state();

        let mut tokens: Vec<Token> = Vec::new();

        // read all tokens
        loop {
            let token = self.read_token();
            tokens.push(token.clone());
            if matches!(token.kind, TokenType::EndOfFile) {
                break;
            }
        }

        // fill the value of each token
        for token in &mut tokens {
            token.value = self.get_string(token.start, token.stop);
        }

        // sort the tokens into whitespace and non-whitespace
        let mut whitespace_buffer: Vec<Token> = Vec::new();
        let mut none_whitespace: Vec<Token> = Vec::new();

        for token in &mut tokens {
            if matches!(token.kind, TokenType::Discard) {
                continue;
            }

            if token.is_whitespace {
                whitespace_buffer.push(token.clone());
            } else {
                token.whitespace = whitespace_buffer.clone();
                none_whitespace.push(token.clone());
                whitespace_buffer.clear();
            }
        }

        tokens = none_whitespace;

        if !tokens.is_empty() {
            let last = tokens.last_mut().unwrap();
            last.value = "".to_string();
        }

        tokens
    }

    fn read_space(&mut self) -> Option<TokenType> {
        if Syntax::is_space(self.get_current_char()) {
            while !self.the_end() {
                self.advance(1);
                if !Syntax::is_space(self.get_current_char()) {
                    break;
                }
            }
            return Some(TokenType::Space);
        }
        None
    }
    fn read_letter(&mut self) -> Option<TokenType> {
        if Syntax::is_letter(self.get_current_char()) {
            while !self.the_end() {
                self.advance(1);
                if !Syntax::is_during_letter(self.get_current_char()) {
                    break;
                }
            }

            return Some(TokenType::Letter);
        }
        None
    }

    fn read_symbol(&mut self) -> Option<TokenType> {
        // TODO: avoid clone
        if self.read_from_array(self.runtime_syntax.symbols.clone()) {
            return Some(TokenType::Symbol);
        }

        // TODO: avoid clone
        if self.read_from_array(self.typesystem_syntax.symbols.clone()) {
            return Some(TokenType::Symbol);
        }
        None
    }

    fn read_comment_escape(&mut self) -> Option<TokenType> {
        if self.is_value("-", 0)
            && self.is_value("-", 1)
            && self.is_value("[", 2)
            && self.is_value("[", 3)
            && self.is_value("#", 4)
        {
            self.advance(5);
            self.comment_escape = true;
            return Some(TokenType::CommentEscape);
        }

        None
    }

    fn read_remaining_comment_escape(&mut self) -> Option<TokenType> {
        if self.comment_escape && self.is_value("]", 0) && self.is_value("]", 1) {
            self.advance(2);
            self.comment_escape = false;
            return Some(TokenType::CommentEscape);
        }

        None
    }

    fn read_multiline_c_comment(&mut self) -> Option<TokenType> {
        if self.is_value("/", 0) && self.is_value("*", 1) {
            self.advance(2);
            while !self.the_end() {
                if self.is_current_value("\n") {
                    break;
                }
                self.advance(1);
            }
            return Some(TokenType::MultilineComment);
        }

        None
    }

    fn read_line_comment(&mut self) -> Option<TokenType> {
        if self.is_value("-", 0) && self.is_value("-", 1) {
            self.advance(2);
            while !self.the_end() {
                if self.is_current_value("\n") {
                    break;
                }
                self.advance(1);
            }
            return Some(TokenType::LineComment);
        }

        None
    }
    fn read_line_c_comment(&mut self) -> Option<TokenType> {
        if self.is_value("/", 0) && self.is_value("/", 1) {
            self.advance(2);
            while !self.the_end() {
                if self.is_current_value("\n") {
                    break;
                }
                self.advance(1);
            }
            return Some(TokenType::LineComment);
        }

        None
    }

    fn read_multiline_comment(&mut self) -> Option<TokenType> {
        if self.is_value("-", 0)
            && self.is_value("-", 1)
            && self.is_value("[", 2)
            && (self.is_value("[", 3) || self.is_value("=", 3))
        {
            let start = self.get_position();
            self.advance(3);

            while self.is_current_value("=") {
                self.advance(1);
            }

            if !self.is_current_value("[") {
                self.set_position(start);
                return None;
            }

            self.advance(1);
            let eq = ("=").repeat(self.get_position() - start - 4);

            if let Some(pos) = self.find_nearest(("]".to_string() + &eq + "]").as_str()) {
                self.set_position(pos);
                return Some(TokenType::MultilineComment);
            }

            self.error(
                "unclosed multiline comment",
                Some(start),
                Some(start + 1),
                None,
            );
            self.set_position(start + 2);
        }

        None
    }

    fn read_analyzer_debug_code(&mut self) -> Option<TokenType> {
        if self.is_value("§", 0) {
            self.advance(2);
            while !self.the_end() {
                if self.is_current_value("\n") {
                    break;
                }
                self.advance(1);
            }
            return Some(TokenType::AnalyzerDebugCode);
        }

        None
    }

    fn read_parser_debug_code(&mut self) -> Option<TokenType> {
        if self.is_value("£", 0) {
            self.advance(2);
            while !self.the_end() {
                if self.is_current_value("\n") {
                    break;
                }
                self.advance(1);
            }
            return Some(TokenType::ParserDebugCode);
        }

        None
    }

    fn read_number_pow_exponent(&mut self, what: &str) -> bool {
        self.advance(1);

        if self.is_current_value("+") || self.is_current_value("-") {
            self.advance(1);

            if !Syntax::is_number(self.get_current_char()) {
                self.error(
                    format!(
                        "malformed {} expected number, got {}",
                        what,
                        self.get_current_char()
                    )
                    .as_str(),
                    Some(self.get_position() - 2),
                    Some(self.get_position() - 1),
                    None,
                );
            }

            while !self.the_end() {
                if !Syntax::is_number(self.get_current_char()) {
                    break;
                }
                self.advance(1);
            }
        }

        true
    }

    fn read_number_annotations(&mut self, what: AnnotationType) -> bool {
        match what {
            AnnotationType::Hex => {
                if self.is_current_value("p") && self.is_current_value("P") {
                    return self.read_number_pow_exponent("pow");
                }
            }
            AnnotationType::Decimal => {
                if self.is_current_value("e") && self.is_current_value("E") {
                    return self.read_number_pow_exponent("exponent");
                }
            }
            AnnotationType::Binary => {}
        }

        // TODO: what about typesystem number annotations?
        // TODO: avoid clone
        self.read_from_array(self.runtime_syntax.number_annotations.clone())
    }

    fn read_hex_number(&mut self) {
        self.advance(2);
        let mut dot = false;

        while !self.the_end() {
            if self.is_current_value("_") {
                self.advance(1);
            }

            if self.is_current_value(".") {
                if dot {
                    self.error(
                        "malformed hex number, got more than one dot",
                        Some(self.get_position() - 1),
                        Some(self.get_position()),
                        None,
                    );
                }
                dot = true;
                self.advance(1);
            }

            if self.read_number_annotations(AnnotationType::Hex) {
                break;
            }

            if self.runtime_syntax.is_valid_hex(self.get_current_char()) {
                self.advance(1);
            } else if Syntax::is_space(self.get_current_char())
                || Syntax::is_symbol(self.get_current_char())
            {
                break;
            } else {
                self.error(
                    format!(
                        "malformed hex number {} in hex notation",
                        self.get_current_char()
                    )
                    .as_str(),
                    Some(self.get_position() - 1),
                    Some(self.get_position()),
                    None,
                );
            }
        }
    }

    fn read_binary_number(&mut self) {
        self.advance(2);

        while !self.the_end() {
            if self.is_current_value("_") {
                self.advance(1);
            }

            if self.is_current_value("1") || self.is_current_value("0") {
                self.advance(1);
            } else if Syntax::is_space(self.get_current_char()) {
                break;
            } else {
                self.error(
                    "malformed binary number, expected 0 or 1",
                    Some(self.get_position() - 1),
                    Some(self.get_position()),
                    None,
                );
                break;
            }

            if self.read_number_annotations(AnnotationType::Binary) {
                break;
            }
        }
    }

    fn read_decimal_number(&mut self) {
        let mut dot = false;

        while !self.the_end() {
            if self.is_current_value("_") {
                self.advance(1);
            }

            if self.is_current_value(".") {
                if self.is_value(".", 1) {
                    return;
                }
                if dot {
                    return;
                }

                dot = true;

                self.advance(1);
            }

            if self.read_number_annotations(AnnotationType::Decimal) {
                break;
            }

            if Syntax::is_number(self.get_current_char()) {
                self.advance(1);
            } else {
                break;
            }
        }
    }

    fn read_number(&mut self) -> Option<TokenType> {
        if Syntax::is_number(self.get_current_char())
            || (self.is_current_value(".")
                && Syntax::is_number(self.get_byte_char_offset(1) as char))
        {
            if self.is_value("x", 1) || self.is_value("X", 1) {
                self.read_hex_number()
            } else if self.is_value("b", 1) || self.is_value("B", 1) {
                self.read_binary_number()
            } else {
                self.read_decimal_number()
            }

            return Some(TokenType::Number);
        }

        None
    }

    fn read_multiline_string(&mut self) -> Option<TokenType> {
        if self.is_value("[", 0) && (self.is_value("[", 1) || self.is_value("=", 1)) {
            let start = self.get_position();
            self.advance(1);

            if self.is_current_value("=") {
                while !self.the_end() {
                    self.advance(1);
                    if !self.is_current_value("=") {
                        break;
                    }
                }
            }

            if !self.is_current_value("=") {
                self.error(
                    "malformed multiline string, expected =",
                    Some(start),
                    Some(self.get_position()),
                    None,
                );
                return None;
            }

            self.advance(1);

            let closing = "]".to_string() + &("=").repeat(self.get_position() - start - 4) + "]";

            if let Some(pos) = self.find_nearest(closing.as_str()) {
                self.set_position(pos);
                return Some(TokenType::MultilineComment);
            }

            self.error(
                "expected multiline string reached end of code",
                Some(start),
                Some(self.get_position()),
                None,
            );
        }
        None
    }

    fn read_quoted_string(&mut self, quote: char) -> Option<TokenType> {
        if !self.is_current_byte(quote as u8) {
            return None;
        }

        let start = self.get_position();
        self.advance(1);

        while !self.the_end() {
            let char = self.read_char();

            if char == '\\' {
                let char = self.read_char();

                if char == 'z' && !self.is_current_value(quote.to_string().as_str()) {
                    self.read_space();
                }
            } else if char == '\n' {
                self.set_position(start);
                self.error(
                    "expected quote to end",
                    Some(start),
                    Some(self.get_position() - 1),
                    None,
                );
            } else if char == quote {
                return Some(TokenType::String);
            }
        }

        self.error(
            "expected quote to end: reached end of file",
            Some(start),
            Some(self.get_position()),
            None,
        );
        None
    }

    fn read_single_quoted_string(&mut self) -> Option<TokenType> {
        self.read_quoted_string('\'')
    }

    fn read_double_quoted_string(&mut self) -> Option<TokenType> {
        self.read_quoted_string('"')
    }
}

fn main() {
    let code = Code {
        buffer: "5.6e3".to_string(),
        name: "test".to_string(),
    };

    let mut runtime_syntax = lua_syntax();
    runtime_syntax.build();

    let mut typesystem_syntax = lua_syntax();
    typesystem_syntax.build();

    let mut lexer = Lexer {
        code,
        position: 0,
        runtime_syntax,
        typesystem_syntax,
        comment_escape: false,
    };

    let tokens = lexer.get_tokens();

    for token in &tokens {
        for wtoken in &token.whitespace {
            print!("{}", wtoken.value);
        }
        print!("{}", token.value);
    }
}

fn tokenize(code_string: String) -> Vec<Token> {
    let code = Code {
        buffer: code_string,
        name: "test".to_string(),
    };

    let mut runtime_syntax = lua_syntax();
    runtime_syntax.build();

    let mut typesystem_syntax = lua_syntax();
    typesystem_syntax.build();

    let mut lexer = Lexer {
        code,
        position: 0,
        runtime_syntax,
        typesystem_syntax,
        comment_escape: false,
    };

    lexer.get_tokens()
}

fn one_token(tokens: Vec<Token>) -> Token {
    if tokens.len() != 2 {
        panic!("expected 1 token, got {}", tokens.len());
    }

    assert!(matches!(tokens[1].kind, TokenType::EndOfFile));

    tokens[0].clone()
}

#[test]
fn unclosed_multiline_comment() {
    assert!(matches!(
        tokenize("".to_string())[0].kind,
        TokenType::EndOfFile
    ));

    assert!(matches!(
        one_token(tokenize("a".to_string())).kind,
        TokenType::Letter
    ));

    assert!(matches!(
        one_token(tokenize("1".to_string())).kind,
        TokenType::Number
    ));

    assert!(matches!(
        one_token(tokenize("(".to_string())).kind,
        TokenType::Symbol
    ));
}

#[test]
fn shebang() {
    assert!(matches!(
        tokenize("#!/usr/bin/env lua".to_string())[0].kind,
        TokenType::Shebang
    ));
}

#[test]
fn single_quote_string() {
    assert!(matches!(
        one_token(tokenize("'1'".to_string())).kind,
        TokenType::String
    ));
}
