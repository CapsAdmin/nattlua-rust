#![allow(dead_code)]
use core::cmp::Reverse;
use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::{error::Error, fmt};

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

#[derive(Eq, PartialEq, Debug)]
pub enum AnnotationType {
    Hex,
    Decimal,
    Binary,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum TokenType {
    AnalyzerDebugCode,
    ParserDebugCode,
    Letter,
    String,
    Number,
    Symbol,
    EndOfFile, // sort of whitespace
    Shebang,   // sort of whitespace
    Discard,
    Unknown,

    LineComment,
    MultilineComment,
    CommentEscape,
    Space,
}

impl TokenType {
    pub fn is_whitespace(&self) -> bool {
        matches!(
            self,
            TokenType::LineComment | TokenType::MultilineComment | TokenType::CommentEscape | TokenType::Space
        )
    }
}

#[derive(Debug)]
struct LexerError {
    message: String,
    start: usize,
    stop: usize,
}

impl Error for LexerError {}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Oh no, something bad went down")
    }
}

#[derive(Clone)]
struct Token {
    kind: TokenType,
    value: String,
    start: usize,
    stop: usize,
    whitespace: Vec<Token>,
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
        ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_' || c == '@' || c >= 127u8 as char
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
            "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f", "A", "B", "C", "D", "E",
            "F",
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

        symbol_characters: string_vec![",", ";", "(", ")", "{", "}", "[", "]", "=", "::", '"', "'", "<|", "|>"],
        number_annotations: string_vec!["ull", "ll", "ul", "i"],
        keywords: string_vec![
            "do", "end", "if", "then", "else", "elseif", "for", "in", "while", "repeat", "until", "break", "return",
            "local", "function", "and", "not", "or", // these are just to make sure all code is covered by tests
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

    fn is_value(&self, value: &str, offset: usize) -> bool {
        let l = self.get_string(self.get_position() + offset, self.get_position() + offset + value.len());

        l == value
    }

    fn error(&self, message: &str, start: Option<usize>, stop: Option<usize>, args: Option<Vec<String>>) {
        let mut buffer = String::new();
        buffer.push_str("Error: ");
        buffer.push_str(message);
        buffer.push('\n');
        buffer.push('\t');
        buffer.push_str(self.code.get_string().as_str());
        buffer.push('\n');
        buffer.push('\t');
        if let Some(start) = start {
            for _ in 0..start {
                buffer.push(' ');
            }
            buffer.push('^');
        }
        buffer.push('\n');
        if let Some(stop) = stop {
            buffer.push('\t');
            for _ in 0..stop {
                buffer.push(' ');
            }
            buffer.push('^');
        }

        if args.is_some() {
            for arg in args.unwrap() {
                buffer.push('\n');
                buffer.push('\t');
                buffer.push_str(arg.as_str());
            }
        }
        println!("{}", buffer);
    }

    fn new_token(kind: TokenType, start: usize, stop: usize) -> Token {
        Token {
            kind,
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

    fn read_whitespace(&mut self) -> Result<Option<TokenType>, LexerError> {
        self.read_space()
            .transpose()
            .or_else(|| self.read_comment_escape().transpose())
            .or_else(|| self.read_multiline_c_comment().transpose())
            .or_else(|| self.read_line_c_comment().transpose())
            .or_else(|| self.read_multiline_comment().transpose())
            .or_else(|| self.read_line_comment().transpose())
            .transpose()
    }

    fn read_non_whitespace(&mut self) -> Result<Option<TokenType>, LexerError> {
        self.read_analyzer_debug_code()
            .transpose()
            .or_else(|| self.read_parser_debug_code().transpose())
            .or_else(|| self.read_number().transpose())
            .or_else(|| self.read_multiline_string().transpose())
            .or_else(|| self.read_single_quoted_string().transpose())
            .or_else(|| self.read_double_quoted_string().transpose())
            .or_else(|| self.read_letter().transpose())
            .or_else(|| self.read_symbol().transpose())
            .transpose()
    }

    fn read_unknown(&mut self) -> Result<Option<TokenType>, LexerError> {
        self.advance(1);
        Ok(Some(TokenType::Unknown))
    }

    fn read_the_end(&mut self) -> Result<Option<TokenType>, LexerError> {
        if self.the_end() {
            return Ok(Some(TokenType::EndOfFile));
        }
        Ok(None)
    }

    fn read_single_token(&mut self) -> Result<Token, LexerError> {
        let start = self.get_position();

        let res = self
            .read_shebang()
            .transpose()
            .or_else(|| self.read_remaining_comment_escape().transpose())
            .or_else(|| self.read_whitespace().transpose())
            .or_else(|| self.read_non_whitespace().transpose())
            .or_else(|| self.read_the_end().transpose())
            .or_else(|| self.read_unknown().transpose())
            .transpose();

        if res.is_err() {
            return Err(res.err().unwrap());
        }

        let kind = res.unwrap().unwrap();

        Ok(Self::new_token(kind, start, self.get_position()))
    }

    fn read_token(&mut self) -> Result<Token, LexerError> {
        let mut whitespace_tokens: Vec<Token> = Vec::new();

        loop {
            let res = self.read_single_token();

            if let Ok(mut token) = res {
                if token.kind.is_whitespace() {
                    whitespace_tokens.push(token);
                } else {
                    for tk in &mut whitespace_tokens {
                        tk.value = self.get_string(tk.start, tk.stop);
                    }
                    token.value = self.get_string(token.start, token.stop);
                    token.whitespace = whitespace_tokens.clone();
                    whitespace_tokens.clear();
                    return Ok(token);
                }
            } else {
                return res;
            }
        }
    }

    fn get_tokens(&mut self) -> (Vec<Token>, Vec<LexerError>) {
        self.reset_state();

        let mut tokens: Vec<Token> = Vec::new();
        let mut errors: Vec<LexerError> = Vec::new();

        loop {
            let res = self.read_token();
            if let Ok(token) = res {
                tokens.push(token.clone());

                if token.kind == TokenType::EndOfFile {
                    break;
                }
            } else {
                errors.push(res.err().unwrap());
            }
        }

        (tokens, errors)
    }

    fn read_shebang(&mut self) -> Result<Option<TokenType>, LexerError> {
        if self.get_position() == 0 && self.is_value("#", 0) {
            while !self.the_end() {
                self.advance(1);

                if self.is_value("\n", 0) {
                    break;
                }
            }
            return Ok(Some(TokenType::Shebang));
        }

        Ok(None)
    }

    fn read_space(&mut self) -> Result<Option<TokenType>, LexerError> {
        if Syntax::is_space(self.get_current_char()) {
            while !self.the_end() {
                self.advance(1);

                if !Syntax::is_space(self.get_current_char()) {
                    break;
                }
            }

            return Ok(Some(TokenType::Space));
        }
        Ok(None)
    }
    fn read_letter(&mut self) -> Result<Option<TokenType>, LexerError> {
        if Syntax::is_letter(self.get_current_char()) {
            while !self.the_end() {
                self.advance(1);

                if !Syntax::is_during_letter(self.get_current_char()) {
                    break;
                }
            }

            return Ok(Some(TokenType::Letter));
        }
        Ok(None)
    }

    fn read_symbol(&mut self) -> Result<Option<TokenType>, LexerError> {
        // TODO: avoid clone
        if self.read_from_array(self.runtime_syntax.symbols.clone()) {
            return Ok(Some(TokenType::Symbol));
        }

        // TODO: avoid clone
        if self.read_from_array(self.typesystem_syntax.symbols.clone()) {
            return Ok(Some(TokenType::Symbol));
        }
        Ok(None)
    }

    fn read_comment_escape(&mut self) -> Result<Option<TokenType>, LexerError> {
        if self.is_value("-", 0)
            && self.is_value("-", 1)
            && self.is_value("[", 2)
            && self.is_value("[", 3)
            && self.is_value("#", 4)
        {
            self.advance(5);
            self.comment_escape = true;
            return Ok(Some(TokenType::CommentEscape));
        }

        Ok(None)
    }

    fn read_remaining_comment_escape(&mut self) -> Result<Option<TokenType>, LexerError> {
        if self.comment_escape && self.is_value("]", 0) && self.is_value("]", 1) {
            self.advance(2);
            self.comment_escape = false;
            return Ok(Some(TokenType::CommentEscape));
        }

        Ok(None)
    }

    fn read_multiline_c_comment(&mut self) -> Result<Option<TokenType>, LexerError> {
        if self.is_value("/", 0) && self.is_value("*", 1) {
            let start = self.get_position();
            self.advance(2);

            while !self.the_end() {
                if self.is_value("*", 0) && self.is_value("/", 1) {
                    self.advance(2);
                    return Ok(Some(TokenType::MultilineComment));
                }

                self.advance(1);
            }

            return Err(LexerError {
                message: "tried to find end of multiline c comment, reached end of code".to_string(),
                start,
                stop: self.get_position(),
            });
        }

        Ok(None)
    }

    fn read_line_comment(&mut self) -> Result<Option<TokenType>, LexerError> {
        if self.is_value("-", 0) && self.is_value("-", 1) {
            self.advance(2);
            while !self.the_end() {
                if self.is_value("\n", 0) {
                    break;
                }
                self.advance(1);
            }
            return Ok(Some(TokenType::LineComment));
        }

        Ok(None)
    }
    fn read_line_c_comment(&mut self) -> Result<Option<TokenType>, LexerError> {
        if self.is_value("/", 0) && self.is_value("/", 1) {
            self.advance(2);
            while !self.the_end() {
                if self.is_value("\n", 0) {
                    break;
                }
                self.advance(1);
            }
            return Ok(Some(TokenType::LineComment));
        }

        Ok(None)
    }

    fn read_multiline_comment(&mut self) -> Result<Option<TokenType>, LexerError> {
        if self.is_value("-", 0)
            && self.is_value("-", 1)
            && self.is_value("[", 2)
            && (self.is_value("[", 3) || self.is_value("=", 3))
        {
            let start = self.get_position();
            self.advance(3);

            while self.is_value("=", 0) {
                self.advance(1);
            }

            // if we have an incomplete multiline comment, it's just a single line comment
            if !self.is_value("[", 0) {
                self.set_position(start);
                return self.read_line_comment();
            }

            self.advance(1);
            let eq = ("=").repeat(self.get_position() - start - 4);

            if let Some(pos) = self.find_nearest(("]".to_string() + &eq + "]").as_str()) {
                self.set_position(pos);
                return Ok(Some(TokenType::MultilineComment));
            }

            self.set_position(start + 2);

            return Err(LexerError {
                message: "unclosed multiline comment".to_string(),
                start,
                stop: start + 1,
            });
        }

        Ok(None)
    }

    fn read_analyzer_debug_code(&mut self) -> Result<Option<TokenType>, LexerError> {
        if self.is_value("§", 0) {
            self.advance(2);
            while !self.the_end() {
                if self.is_value("\n", 0) {
                    break;
                }
                self.advance(1);
            }
            return Ok(Some(TokenType::AnalyzerDebugCode));
        }

        Ok(None)
    }

    fn read_parser_debug_code(&mut self) -> Result<Option<TokenType>, LexerError> {
        if self.is_value("£", 0) {
            self.advance(2);
            while !self.the_end() {
                if self.is_value("\n", 0) {
                    break;
                }
                self.advance(1);
            }
            return Ok(Some(TokenType::ParserDebugCode));
        }

        Ok(None)
    }

    fn read_number_exponent(&mut self, what: &str) -> Result<bool, LexerError> {
        // skip the 'e', 'E', 'p' or 'P'
        self.advance(1);

        if self.is_value("+", 0) || self.is_value("-", 0) {
            self.advance(1);
        } else {
            return Err(LexerError {
                message: format!("expected '+' or '-' after '{}'", what),
                start: self.get_position() - 1,
                stop: self.get_position(),
            });
        }

        if !Syntax::is_number(self.get_current_char()) {
            return Err(LexerError {
                message: format!("malformed {} expected number, got {}", what, self.get_current_char()),
                start: self.get_position() - 2,
                stop: self.get_position() - 1,
            });
        }

        while !self.the_end() {
            if !Syntax::is_number(self.get_current_char()) {
                break;
            }

            self.advance(1);
        }

        Ok(true)
    }

    fn read_hex_number(&mut self) -> Result<Option<TokenType>, LexerError> {
        // 0xABCDEF_1234567890.100p+10

        if self.is_value("0", 0) && (self.is_value("x", 1) || self.is_value("X", 1)) {
            // skip past 0x
            self.advance(2);

            while !self.the_end() {
                if self.is_value("_", 0) {
                    self.advance(1);
                }

                if self.is_value(".", 0) && !self.is_value(".", 1) {
                    self.advance(1);
                }

                if self.runtime_syntax.is_valid_hex(self.get_current_char()) {
                    self.advance(1);
                } else {
                    if Syntax::is_space(self.get_current_char()) || Syntax::is_symbol(self.get_current_char()) {
                        break;
                    }

                    if self.is_value("p", 0) && self.is_value("P", 0) && self.read_number_exponent("pow").is_ok() {
                        break;
                    }

                    return Err(LexerError {
                        message: format!("malformed hex number {} in hex notation", self.get_current_char()),
                        start: self.get_position() - 1,
                        stop: self.get_position(),
                    });
                }
            }

            self.read_from_array(self.runtime_syntax.number_annotations.clone());

            return Ok(Some(TokenType::Number));
        }

        Ok(None)
    }

    fn read_binary_number(&mut self) -> Result<Option<TokenType>, LexerError> {
        if self.is_value("0", 0) && (self.is_value("b", 1) || self.is_value("B", 1)) {
            self.advance(2);

            while !self.the_end() {
                // EXTENSION: allow numbers to be separated by _
                if self.is_value("_", 0) {
                    self.advance(1);
                }

                if self.is_value("1", 0) || self.is_value("0", 0) {
                    self.advance(1);
                } else {
                    if Syntax::is_space(self.get_current_char()) || Syntax::is_symbol(self.get_current_char()) {
                        break;
                    }

                    if self.read_from_array(self.runtime_syntax.number_annotations.clone()) {
                        break;
                    }

                    return Err(LexerError {
                        message: "malformed binary number, expected 0, 1 or space".to_string(),
                        start: self.get_position() - 1,
                        stop: self.get_position(),
                    });
                }
            }

            return Ok(Some(TokenType::Number));
        }

        Ok(None)
    }

    fn read_decimal_number(&mut self) -> Result<Option<TokenType>, LexerError> {
        if Syntax::is_number(self.get_current_char())
            || (self.is_value(".", 0) && Syntax::is_number(self.get_byte_char_offset(1) as char))
        {
            if self.is_value(".", 0) {
                self.advance(1);
            }

            while !self.the_end() {
                if self.is_value("_", 0) {
                    self.advance(1);
                }

                if Syntax::is_number(self.get_current_char()) {
                    self.advance(1);
                } else {
                    if Syntax::is_space(self.get_current_char()) || Syntax::is_symbol(self.get_current_char()) {
                        break;
                    }

                    if self.is_value("e", 0) || self.is_value("E", 0) {
                        if let Err(err) = self.read_number_exponent("exponent") {
                            return Err(err);
                        }
                        break;
                    }

                    if self.read_from_array(self.runtime_syntax.number_annotations.clone()) {
                        break;
                    }

                    return Err(LexerError {
                        message: "malformed decimal number".to_string(),
                        start: self.get_position() - 1,
                        stop: self.get_position(),
                    });
                }
            }

            return Ok(Some(TokenType::Number));
        }
        Ok(None)
    }

    fn read_number(&mut self) -> Result<Option<TokenType>, LexerError> {
        if Syntax::is_number(self.get_current_char())
            || (self.is_value(".", 0) && Syntax::is_number(self.get_byte_char_offset(1) as char))
        {
            if self.is_value("x", 1) || self.is_value("X", 1) {
                return self.read_hex_number();
            } else if self.is_value("b", 1) || self.is_value("B", 1) {
                return self.read_binary_number();
            }
            return self.read_decimal_number();
        }

        Ok(None)
    }

    fn read_multiline_string(&mut self) -> Result<Option<TokenType>, LexerError> {
        if self.is_value("[", 0) && (self.is_value("[", 1) || self.is_value("=", 1)) {
            let start = self.get_position();
            self.advance(1);

            if self.is_value("=", 0) {
                while !self.the_end() {
                    self.advance(1);
                    if !self.is_value("=", 0) {
                        break;
                    }
                }
            }

            if !self.is_value("[", 0) {
                return Err(LexerError {
                    message: "malformed multiline string, expected =".to_string(),
                    start,
                    stop: self.get_position(),
                });
            }

            self.advance(1);

            let pos = self.get_position();

            let closing = "]".to_string() + &("=").repeat(pos - start - 2) + "]";

            if let Some(pos2) = self.find_nearest(closing.as_str()) {
                self.set_position(pos + pos2 + closing.len() - 1);
                return Ok(Some(TokenType::MultilineComment));
            }

            return Err(LexerError {
                message: "expected multiline string reached end of code".to_string(),
                start,
                stop: self.get_position(),
            });
        }
        Ok(None)
    }

    fn read_quoted_string(&mut self, quote: char) -> Result<Option<TokenType>, LexerError> {
        if !self.is_byte(quote as u8, 0) {
            return Ok(None);
        }

        let start = self.get_position();
        self.advance(1);

        while !self.the_end() {
            let char = self.read_char();

            if char == '\\' {
                let char = self.read_char();

                if char == 'z' && !self.is_value(quote.to_string().as_str(), 0) {
                    if let Err(err) = self.read_space() {
                        return Err(err);
                    }
                }
            } else if char == '\n' {
                return Err(LexerError {
                    message: "expected quote to end".to_string(),
                    start,
                    stop: self.get_position(),
                });
            } else if char == quote {
                return Ok(Some(TokenType::String));
            }
        }

        Err(LexerError {
            message: "expected quote to end: reached end of file".to_string(),
            start,
            stop: self.get_position() - 1,
        })
    }

    fn read_single_quoted_string(&mut self) -> Result<Option<TokenType>, LexerError> {
        self.read_quoted_string('\'')
    }

    fn read_double_quoted_string(&mut self) -> Result<Option<TokenType>, LexerError> {
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

    let mut typesystem_syntax = lua_typesystem_syntax();
    typesystem_syntax.build();

    let mut lexer = Lexer {
        code,
        position: 0,
        runtime_syntax,
        typesystem_syntax,
        comment_escape: false,
    };

    let (tokens, _) = lexer.get_tokens();

    for token in &tokens {
        for whitespace_token in &token.whitespace {
            print!("{}", whitespace_token.value);
        }
        print!("{}", token.value);
    }
}

fn tokenize(code_string: &str) -> Vec<Token> {
    let code = Code {
        buffer: code_string.to_string(),
        name: "test".to_string(),
    };

    let mut runtime_syntax = lua_syntax();
    runtime_syntax.build();

    let mut typesystem_syntax = lua_typesystem_syntax();
    typesystem_syntax.build();

    let mut lexer = Lexer {
        code,
        position: 0,
        runtime_syntax,
        typesystem_syntax,
        comment_escape: false,
    };

    let (tokens, errors) = lexer.get_tokens();

    for error in &errors {
        println!("{}", error.message);
    }

    tokens
}

fn one_token(tokens: Vec<Token>) -> Token {
    if tokens.len() != 2 {
        panic!("expected 1 token, got {}", tokens.len());
    }

    assert_eq!(tokens[1].kind, TokenType::EndOfFile);

    tokens[0].clone()
}

fn tokens_to_string(tokens: Vec<Token>) -> String {
    let mut result = String::new();

    for token in &tokens {
        for whitespace_token in &token.whitespace {
            result.push_str(&whitespace_token.value);
        }
        result.push_str(&token.value);
    }

    result
}

fn expect_error(code_string: &str, expected_error: &str) -> Vec<Token> {
    let code = Code {
        buffer: code_string.to_string(),
        name: "test".to_string(),
    };

    let mut runtime_syntax = lua_syntax();
    runtime_syntax.build();

    let mut typesystem_syntax = lua_typesystem_syntax();
    typesystem_syntax.build();

    let mut lexer = Lexer {
        code,
        position: 0,
        runtime_syntax,
        typesystem_syntax,
        comment_escape: false,
    };

    let (tokens, errors) = lexer.get_tokens();

    for error in &errors {
        if error.message.contains(expected_error) {
            return tokens;
        }
    }

    println!("could not find error {} got these instead:", expected_error);

    for error in &errors {
        println!("\n{}", error.message);
    }

    panic!("expected error, got no errors");
}

fn check(code: &str) {
    let actual = tokens_to_string(tokenize(&code));

    assert_eq!(actual, code);
}

#[test]
fn tokens_to_string_test() {
    check("local foo =   5 + 2..2");
}

#[test]
fn unclosed_multiline_comment() {
    assert_eq!(tokenize("")[0].kind, TokenType::EndOfFile);
    assert_eq!(one_token(tokenize("a")).kind, TokenType::Letter);
    assert_eq!(one_token(tokenize("1")).kind, TokenType::Number);
    assert_eq!(one_token(tokenize("(")).kind, TokenType::Symbol);
}

#[test]
fn shebang() {
    assert_eq!(tokenize("#!/usr/bin/env lua")[0].kind, TokenType::Shebang);
}

#[test]
fn single_quote_string() {
    assert_eq!(one_token(tokenize("'1'")).kind, TokenType::String);
}

#[test]
fn z_escaped_string() {
    assert_eq!(one_token(tokenize("\"a\\z\na\"")).kind, TokenType::String);
}

#[test]
fn number_range() {
    assert_eq!(tokenize("1..20").len(), 4);
}
#[test]
fn number_delimiter() {
    assert_eq!(tokenize("1_000_000").len(), 2);
    assert_eq!(tokenize("0xdead_beef").len(), 2);
    assert_eq!(tokenize("0b0101_0101").len(), 2);
}

#[test]
fn number_annotations() {
    assert_eq!(tokenize("50ull").len(), 2);
    assert_eq!(tokenize("50uLL").len(), 2);
    assert_eq!(tokenize("50ULL").len(), 2);
    assert_eq!(tokenize("50LL").len(), 2);
    assert_eq!(tokenize("50lL").len(), 2);
}

#[test]
fn malformed_number() {
    expect_error("12LOL", "malformed decimal number");
    expect_error("0xbLOL", "malformed hex number");
    expect_error("0b101LOL01", "malformed binary number");
    expect_error("1.5eD", "after 'exponent'");
}

#[test]
fn multiline_comment_error() {
    expect_error("/*", "tried to find end of multiline c comment");
    expect_error("--[[", "unclosed multiline comment");
}

#[test]
fn string_error() {
    expect_error("\"woo\nfoo", "expected quote to end");
    expect_error("'aaa", "expected quote to end: reached end of file");
}

#[test]
fn multiline_string() {
    assert_eq!(tokenize("a = [[a]]").len(), 4);
    assert_eq!(tokenize("a = [=[a]=]").len(), 4);
    assert_eq!(tokenize("a = [==[a]==]").len(), 4);

    expect_error("a = [=a", "malformed multiline string");
    expect_error("a = [[a", "expected multiline string reached end of code");
}
