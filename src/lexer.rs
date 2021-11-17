use crate::code::Code;
use crate::syntax::Syntax;
use crate::token::{Token, TokenType};
use std::{error::Error, fmt};

#[derive(Debug)]
struct LexerError {
    message: String,
    start: usize,
    stop: usize,
}

impl Error for LexerError {}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

struct Lexer<'a> {
    pub code: Code<'a>,
    pub position: usize,
    pub runtime_syntax: Syntax,
    pub typesystem_syntax: Syntax,
    pub comment_escape: bool,
}

impl Lexer<'_> {
    fn get_length(&self) -> usize {
        self.code.len()
    }

    fn get_string(&self, start: usize, stop: usize) -> &str {
        self.code.substring(start, stop)
    }

    fn get_char_offset(&self, offset: usize) -> char {
        self.code.get_char(self.position + offset)
    }

    fn get_current_char(&self) -> char {
        self.get_char_offset(0) as char
    }

    fn reset_state(&mut self) -> &mut Self {
        self.position = 0;
        self
    }

    fn find_nearest(&self, str: &str) -> Option<usize> {
        self.code.find_nearest(str, self.position)
    }

    fn advance(&mut self, offset: usize) -> &mut Self {
        self.position += offset;
        self
    }

    fn read_char(&mut self) -> char {
        let char = self.get_current_char();
        self.advance(1);
        char
    }
    fn the_end(&self) -> bool {
        self.position >= self.get_length()
    }

    fn is_value(&self, value: &str, offset: usize) -> bool {
        let l = self.get_string(self.position + offset, self.position + offset + value.len());

        l == value
    }
    fn is_char(&self, char: char, offset: usize) -> bool {
        self.is_value(char.to_string().as_str(), offset)
    }

    fn error(&self, message: &str, start: Option<usize>, stop: Option<usize>, args: Option<Vec<String>>) {}

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
                .get_string(self.position, self.position + annotation.len())
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
        let start = self.position;

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

        Ok(Self::new_token(kind, start, self.position))
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
                        tk.value = self.get_string(tk.start, tk.stop).to_string();
                    }
                    token.value = self.get_string(token.start, token.stop).to_string();
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
        if self.position == 0 && self.is_value("#", 0) {
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
        if self.is_value("--[[#", 0) {
            self.advance(5);
            self.comment_escape = true;
            return Ok(Some(TokenType::CommentEscape));
        }

        Ok(None)
    }

    fn read_remaining_comment_escape(&mut self) -> Result<Option<TokenType>, LexerError> {
        if self.comment_escape && self.is_value("]]", 0) {
            self.advance(2);
            self.comment_escape = false;
            return Ok(Some(TokenType::CommentEscape));
        }

        Ok(None)
    }

    fn read_multiline_c_comment(&mut self) -> Result<Option<TokenType>, LexerError> {
        if self.is_value("/*", 0) {
            let start = self.position;
            self.advance(2);

            while !self.the_end() {
                if self.is_value("*/", 0) {
                    self.advance(2);
                    return Ok(Some(TokenType::MultilineComment));
                }

                self.advance(1);
            }

            return Err(LexerError {
                message: "tried to find end of multiline c comment, reached end of code".to_string(),
                start,
                stop: self.position,
            });
        }

        Ok(None)
    }

    fn read_line_comment(&mut self) -> Result<Option<TokenType>, LexerError> {
        if self.is_value("--", 0) {
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
        if self.is_value("//", 0) {
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
        if self.is_value("--[", 0) && (self.is_value("[", 3) || self.is_value("=", 3)) {
            let start = self.position;
            self.advance(3);

            while self.is_value("=", 0) {
                self.advance(1);
            }

            // if we have an incomplete multiline comment, it's just a single line comment
            if !self.is_value("[", 0) {
                self.position = start;
                return self.read_line_comment();
            }

            self.advance(1);
            let pos = self.position;

            let closing = "]".to_string() + &("=").repeat(pos - start - 4) + "]";

            if let Some(pos2) = self.find_nearest(closing.as_str()) {
                self.position = pos + pos2 + closing.len();
                return Ok(Some(TokenType::MultilineComment));
            }

            self.position = start + 2;

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
                start: self.position - 1,
                stop: self.position,
            });
        }

        if !Syntax::is_number(self.get_current_char()) {
            return Err(LexerError {
                message: format!("malformed {} expected number, got {}", what, self.get_current_char()),
                start: self.position - 2,
                stop: self.position - 1,
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
                        start: self.position - 1,
                        stop: self.position,
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
                        start: self.position - 1,
                        stop: self.position,
                    });
                }
            }

            return Ok(Some(TokenType::Number));
        }

        Ok(None)
    }

    fn read_decimal_number(&mut self) -> Result<Option<TokenType>, LexerError> {
        if Syntax::is_number(self.get_current_char())
            || (self.is_value(".", 0) && Syntax::is_number(self.get_char_offset(1) as char))
        {
            let mut has_dot = false;
            if self.is_value(".", 0) {
                self.advance(1);
                has_dot = true;
            }

            while !self.the_end() {
                if self.is_value("_", 0) {
                    self.advance(1);
                }

                if !has_dot && self.is_value(".", 0) {
                    if self.is_value(".", 1) {
                        break;
                    }
                    self.advance(1);
                    has_dot = true;
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
                        start: self.position - 1,
                        stop: self.position,
                    });
                }
            }

            return Ok(Some(TokenType::Number));
        }
        Ok(None)
    }

    fn read_number(&mut self) -> Result<Option<TokenType>, LexerError> {
        self.read_hex_number()
            .transpose()
            .or_else(|| self.read_binary_number().transpose())
            .or_else(|| self.read_decimal_number().transpose())
            .transpose()
    }

    fn read_multiline_string(&mut self) -> Result<Option<TokenType>, LexerError> {
        if self.is_value("[", 0) && (self.is_value("[", 1) || self.is_value("=", 1)) {
            let start = self.position;
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
                    stop: self.position,
                });
            }

            self.advance(1);

            let pos = self.position;

            let closing = "]".to_string() + &("=").repeat(pos - start - 2) + "]";

            if let Some(pos2) = self.find_nearest(closing.as_str()) {
                self.position = pos + pos2 + closing.len();
                return Ok(Some(TokenType::MultilineComment));
            }

            return Err(LexerError {
                message: "expected multiline string reached end of code".to_string(),
                start,
                stop: self.position,
            });
        }
        Ok(None)
    }

    fn read_quoted_string(&mut self, quote: char) -> Result<Option<TokenType>, LexerError> {
        if !self.is_char(quote, 0) {
            return Ok(None);
        }

        let start = self.position;
        self.advance(1);

        while !self.the_end() {
            let char = self.read_char();

            if char == '\\' {
                let char = self.read_char();

                if char == 'z' && !self.is_char(quote, 0) {
                    if let Err(err) = self.read_space() {
                        return Err(err);
                    }
                }
            } else if char == '\n' {
                return Err(LexerError {
                    message: "expected quote to end".to_string(),
                    start,
                    stop: self.position,
                });
            } else if char == quote {
                return Ok(Some(TokenType::String));
            }
        }

        Err(LexerError {
            message: "expected quote to end: reached end of file".to_string(),
            start,
            stop: self.position - 1,
        })
    }

    fn read_single_quoted_string(&mut self) -> Result<Option<TokenType>, LexerError> {
        self.read_quoted_string('\'')
    }

    fn read_double_quoted_string(&mut self) -> Result<Option<TokenType>, LexerError> {
        self.read_quoted_string('"')
    }
}

mod tests {
    use crate::code::Code;
    use crate::lexer::Lexer;
    use crate::syntax::{lua_syntax, lua_typesystem_syntax};
    use crate::token::{Token, TokenType};

    fn tokenize(code_string: &str) -> Vec<Token> {
        let code = Code {
            buffer: code_string,
            name: "test",
        };

        let runtime_syntax = lua_syntax();
        let typesystem_syntax = lua_typesystem_syntax();

        let mut lexer = Lexer {
            code,
            position: 0,
            runtime_syntax,
            typesystem_syntax,
            comment_escape: false,
        };

        let (tokens, errors) = lexer.get_tokens();

        for error in &errors {
            println!("{}", error);
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
            buffer: code_string,
            name: "test",
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
            println!("\t{}", error);
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
        assert_eq!(tokenize("1.5e+20").len(), 2);
        assert_eq!(tokenize(".0").len(), 2);
    }

    #[test]
    fn malformed_number() {
        expect_error("12LOL", "malformed decimal number");
        expect_error("0xbLOL", "malformed hex number");
        expect_error("0b101LOL01", "malformed binary number");
        expect_error("1.5eD", "after 'exponent'");
        expect_error("1.5e+D", "malformed exponent expected number, got D");
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
        assert_eq!(tokenize("a = [[a]]").len(), 3);
        assert_eq!(tokenize("a = [=[a]=]").len(), 3);
        assert_eq!(tokenize("a = [==[a]==]").len(), 3);

        expect_error("a = [=a", "malformed multiline string");
        expect_error("a = [[a", "expected multiline string reached end of code");
    }

    #[test]
    fn multiline_comment() {
        assert_eq!(tokenize("--[[a]]")[0].kind, TokenType::EndOfFile);
        assert_eq!(tokenize("--[=[a]=]")[0].kind, TokenType::EndOfFile);
        assert_eq!(tokenize("--[==[a]==]")[0].kind, TokenType::EndOfFile);
        assert_eq!(tokenize("/*a*/")[0].kind, TokenType::EndOfFile);
    }

    #[test]
    fn line_comment() {
        assert_eq!(tokenize("-- a")[0].kind, TokenType::EndOfFile);
        assert_eq!(tokenize("// a")[0].kind, TokenType::EndOfFile);
        assert_eq!(tokenize("--[= a")[0].kind, TokenType::EndOfFile);
    }

    #[test]
    fn comment_escape() {
        assert_eq!(one_token(tokenize("--[[# 1337 ]]")).kind, TokenType::Number);
    }

    #[test]
    fn typesystem_symbols() {
        assert_eq!(tokenize("$'foo'").len(), 3);
    }
    #[test]
    fn unknown_symbols() {
        assert_eq!(tokenize("```").len(), 4);
    }

    #[test]
    fn debug_code() {
        assert_eq!(one_token(tokenize("§foo = true")).kind, TokenType::AnalyzerDebugCode);
        assert_eq!(one_token(tokenize("£foo = true")).kind, TokenType::ParserDebugCode);
    }
}
