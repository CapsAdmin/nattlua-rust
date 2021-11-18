use crate::code::Code;
use crate::lua_syntax::{runtime_syntax, typesystem_syntax};
use crate::syntax::Syntax;
use crate::token::{Token, TokenType};
use std::fmt;

#[derive(Debug)]
pub struct LexerError {
    message: String,
    start: usize,
    stop: usize,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub struct Lexer<'a> {
    pub code: Code<'a>,
    pub position: usize,
    pub runtime_syntax: Syntax,
    pub typesystem_syntax: Syntax,
    pub comment_escape: bool,
}

type TokenResult = Result<Option<TokenType>, LexerError>;

impl Lexer<'_> {
    fn len(&self) -> usize {
        self.code.len()
    }

    fn get_string(&self, start: usize, stop: usize) -> &str {
        self.code.get_string(start, stop)
    }

    fn char_offset(&self, relative_offset: usize) -> char {
        self.code.get_char(self.position + relative_offset)
    }

    fn is_string(&self, value: &str, relative_offset: usize) -> bool {
        let l = self.get_string(
            self.position + relative_offset,
            self.position + relative_offset + value.len(),
        );

        l == value
    }

    fn reset_state(&mut self) {
        self.position = 0;
    }

    fn find_nearest(&self, str: &str) -> Option<usize> {
        self.code.find_nearest(str, self.position)
    }

    fn advance(&mut self, offset: usize) {
        self.position += offset;
    }

    fn read_char(&mut self) -> char {
        let char = self.char_offset(0);
        self.advance(1);
        char
    }

    fn the_end(&self) -> bool {
        self.position >= self.len()
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

    fn read_whitespace(&mut self) -> TokenResult {
        self.read_space()
            .transpose()
            .or_else(|| self.read_comment_escape().transpose())
            .or_else(|| self.read_multiline_c_comment().transpose())
            .or_else(|| self.read_line_c_comment().transpose())
            .or_else(|| self.read_multiline_comment().transpose())
            .or_else(|| self.read_line_comment().transpose())
            .transpose()
    }

    fn read_non_whitespace(&mut self) -> TokenResult {
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

    fn read_unknown(&mut self) -> TokenResult {
        self.advance(1);
        Ok(Some(TokenType::Unknown))
    }

    fn read_the_end(&mut self) -> TokenResult {
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

    pub fn get_tokens(&mut self) -> (Vec<Token>, Vec<LexerError>) {
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

    fn read_shebang(&mut self) -> TokenResult {
        if self.position != 0 || !self.is_string("#", 0) {
            return Ok(None);
        }

        while !self.the_end() {
            self.advance(1);

            if self.is_string("\n", 0) {
                break;
            }
        }

        Ok(Some(TokenType::Shebang))
    }

    fn read_space(&mut self) -> TokenResult {
        if !Syntax::is_space(self.char_offset(0)) {
            return Ok(None);
        }

        while !self.the_end() {
            self.advance(1);

            if !Syntax::is_space(self.char_offset(0)) {
                break;
            }
        }

        Ok(Some(TokenType::Space))
    }

    fn read_letter(&mut self) -> TokenResult {
        if !Syntax::is_letter(self.char_offset(0)) {
            return Ok(None);
        }

        while !self.the_end() {
            self.advance(1);

            if !Syntax::is_during_letter(self.char_offset(0)) {
                break;
            }
        }

        Ok(Some(TokenType::Letter))
    }

    fn read_symbol(&mut self) -> TokenResult {
        // TODO: avoid clone
        if self.read_from_array(self.runtime_syntax.symbols()) {
            return Ok(Some(TokenType::Symbol));
        }

        // TODO: avoid clone
        if self.read_from_array(self.typesystem_syntax.symbols()) {
            return Ok(Some(TokenType::Symbol));
        }

        Ok(None)
    }

    fn read_comment_escape(&mut self) -> TokenResult {
        if !self.is_string("--[[#", 0) {
            return Ok(None);
        }

        self.advance(5);
        self.comment_escape = true;

        Ok(Some(TokenType::CommentEscape))
    }

    fn read_remaining_comment_escape(&mut self) -> TokenResult {
        if !self.comment_escape || !self.is_string("]]", 0) {
            return Ok(None);
        }

        self.advance(2);
        self.comment_escape = false;
        Ok(Some(TokenType::CommentEscape))
    }

    fn read_multiline_c_comment(&mut self) -> TokenResult {
        if !self.is_string("/*", 0) {
            return Ok(None);
        }

        let start = self.position;
        self.advance(2);

        while !self.the_end() {
            if self.is_string("*/", 0) {
                self.advance(2);
                return Ok(Some(TokenType::MultilineComment));
            }

            self.advance(1);
        }

        Err(LexerError {
            message: "tried to find end of multiline c comment, reached end of code".to_string(),
            start,
            stop: self.position,
        })
    }

    fn read_line_comment(&mut self) -> TokenResult {
        if !self.is_string("--", 0) {
            return Ok(None);
        }

        self.advance(2);
        while !self.the_end() {
            if self.is_string("\n", 0) {
                break;
            }
            self.advance(1);
        }
        Ok(Some(TokenType::LineComment))
    }
    fn read_line_c_comment(&mut self) -> TokenResult {
        if !self.is_string("//", 0) {
            return Ok(None);
        }

        self.advance(2);
        while !self.the_end() {
            if self.is_string("\n", 0) {
                break;
            }
            self.advance(1);
        }

        Ok(Some(TokenType::LineComment))
    }

    fn read_multiline_comment(&mut self) -> TokenResult {
        if !self.is_string("--[", 0) || (!self.is_string("[", 3) && !self.is_string("=", 3)) {
            return Ok(None);
        }

        let start = self.position;
        self.advance(3);

        while self.is_string("=", 0) {
            self.advance(1);
        }

        // if we have an incomplete multiline comment, it's just a single line comment
        if !self.is_string("[", 0) {
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

        Err(LexerError {
            message: "unclosed multiline comment".to_string(),
            start,
            stop: start + 1,
        })
    }

    fn read_analyzer_debug_code(&mut self) -> TokenResult {
        if !self.is_string("§", 0) {
            return Ok(None);
        }

        self.advance(2);
        while !self.the_end() {
            if self.is_string("\n", 0) {
                break;
            }
            self.advance(1);
        }

        Ok(Some(TokenType::AnalyzerDebugCode))
    }

    fn read_parser_debug_code(&mut self) -> TokenResult {
        if !self.is_string("£", 0) {
            return Ok(None);
        }

        self.advance(2);
        while !self.the_end() {
            if self.is_string("\n", 0) {
                break;
            }
            self.advance(1);
        }

        Ok(Some(TokenType::ParserDebugCode))
    }

    fn read_number_exponent(&mut self, what: &str) -> Result<bool, LexerError> {
        // skip the 'e', 'E', 'p' or 'P'
        self.advance(1);

        if self.is_string("+", 0) || self.is_string("-", 0) {
            self.advance(1);
        } else {
            return Err(LexerError {
                message: format!("expected '+' or '-' after '{}'", what),
                start: self.position - 1,
                stop: self.position,
            });
        }

        if !Syntax::is_number(self.char_offset(0)) {
            return Err(LexerError {
                message: format!("malformed {} expected number, got {}", what, self.char_offset(0)),
                start: self.position - 2,
                stop: self.position - 1,
            });
        }

        while !self.the_end() {
            if !Syntax::is_number(self.char_offset(0)) {
                break;
            }

            self.advance(1);
        }

        Ok(true)
    }

    fn read_hex_number(&mut self) -> TokenResult {
        // 0xABCDEF_1234567890.100p+10

        if !self.is_string("0", 0) || (!self.is_string("x", 1) && !self.is_string("X", 1)) {
            return Ok(None);
        }

        // skip past 0x
        self.advance(2);

        while !self.the_end() {
            if self.is_string("_", 0) {
                self.advance(1);
            }

            if self.is_string(".", 0) && !self.is_string(".", 1) {
                self.advance(1);
            }

            if self.runtime_syntax.is_valid_hex(self.char_offset(0)) {
                self.advance(1);
            } else {
                if Syntax::is_space(self.char_offset(0)) || Syntax::is_symbol(self.char_offset(0)) {
                    break;
                }

                if self.is_string("p", 0) && self.is_string("P", 0) && self.read_number_exponent("pow").is_ok() {
                    break;
                }

                return Err(LexerError {
                    message: format!("malformed hex number {} in hex notation", self.char_offset(0)),
                    start: self.position - 1,
                    stop: self.position,
                });
            }
        }

        self.read_from_array(self.runtime_syntax.number_annotations());

        Ok(Some(TokenType::Number))
    }

    fn read_binary_number(&mut self) -> TokenResult {
        if !self.is_string("0", 0) || (!self.is_string("b", 1) && !self.is_string("B", 1)) {
            return Ok(None);
        }

        self.advance(2);

        while !self.the_end() {
            // EXTENSION: allow numbers to be separated by _
            if self.is_string("_", 0) {
                self.advance(1);
            }

            if self.is_string("1", 0) || self.is_string("0", 0) {
                self.advance(1);
            } else {
                if Syntax::is_space(self.char_offset(0)) || Syntax::is_symbol(self.char_offset(0)) {
                    break;
                }

                if self.read_from_array(self.runtime_syntax.number_annotations()) {
                    break;
                }

                return Err(LexerError {
                    message: "malformed binary number, expected 0, 1 or space".to_string(),
                    start: self.position - 1,
                    stop: self.position,
                });
            }
        }

        Ok(Some(TokenType::Number))
    }

    fn read_decimal_number(&mut self) -> TokenResult {
        if !Syntax::is_number(self.char_offset(0))
            && (!self.is_string(".", 0) || !Syntax::is_number(self.char_offset(1) as char))
        {
            return Ok(None);
        }

        let mut has_dot = false;
        if self.is_string(".", 0) {
            self.advance(1);
            has_dot = true;
        }

        while !self.the_end() {
            if self.is_string("_", 0) {
                self.advance(1);
            }

            if !has_dot && self.is_string(".", 0) {
                if self.is_string(".", 1) {
                    break;
                }
                self.advance(1);
                has_dot = true;
            }

            if Syntax::is_number(self.char_offset(0)) {
                self.advance(1);
            } else {
                if Syntax::is_space(self.char_offset(0)) || Syntax::is_symbol(self.char_offset(0)) {
                    break;
                }

                if self.is_string("e", 0) || self.is_string("E", 0) {
                    if let Err(err) = self.read_number_exponent("exponent") {
                        return Err(err);
                    }
                    break;
                }

                if self.read_from_array(self.runtime_syntax.number_annotations()) {
                    break;
                }

                return Err(LexerError {
                    message: "malformed decimal number".to_string(),
                    start: self.position - 1,
                    stop: self.position,
                });
            }
        }

        Ok(Some(TokenType::Number))
    }

    fn read_number(&mut self) -> TokenResult {
        self.read_hex_number()
            .transpose()
            .or_else(|| self.read_binary_number().transpose())
            .or_else(|| self.read_decimal_number().transpose())
            .transpose()
    }

    fn read_multiline_string(&mut self) -> TokenResult {
        if !self.is_string("[", 0) || (!self.is_string("[", 1) && !self.is_string("=", 1)) {
            return Ok(None);
        }

        let start = self.position;
        self.advance(1);

        if self.is_string("=", 0) {
            while !self.the_end() {
                self.advance(1);
                if !self.is_string("=", 0) {
                    break;
                }
            }
        }

        if !self.is_string("[", 0) {
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

        Err(LexerError {
            message: "expected multiline string reached end of code".to_string(),
            start,
            stop: self.position,
        })
    }

    fn read_quoted_string(&mut self, quote: char) -> TokenResult {
        if self.char_offset(0) != quote {
            return Ok(None);
        }

        let start = self.position;
        self.advance(1);

        while !self.the_end() {
            let char = self.read_char();

            if char == '\\' {
                let char = self.read_char();

                if char == 'z' && self.char_offset(0) != quote {
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

    fn read_single_quoted_string(&mut self) -> TokenResult {
        self.read_quoted_string('\'')
    }

    fn read_double_quoted_string(&mut self) -> TokenResult {
        self.read_quoted_string('"')
    }

    pub fn new(code: Code) -> Lexer {
        Lexer {
            code,
            position: 0,
            runtime_syntax: runtime_syntax(),
            typesystem_syntax: typesystem_syntax(),
            comment_escape: false,
        }
    }
}
#[cfg(test)]
mod tests {
    use crate::code::Code;
    use crate::lexer::Lexer;
    use crate::token::{Token, TokenType};

    fn tokenize(contents: &str) -> Vec<Token> {
        let code = Code::new(contents, "test");
        let mut lexer = Lexer::new(code);

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

    fn expect_error(contents: &str, expected_error: &str) -> Vec<Token> {
        let code = Code::new(contents, "test");
        let mut lexer = Lexer::new(code);

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
        let actual = tokens_to_string(tokenize(code));

        assert_eq!(actual, code);
    }

    #[test]
    fn tokens_to_string_test() {
        check("local foo =   5 + 2..2");
    }

    #[test]
    fn smoke() {
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
