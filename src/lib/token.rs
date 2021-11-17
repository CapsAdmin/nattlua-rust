use std::fmt;
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

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenType,
    pub value: String,
    pub start: usize,
    pub stop: usize,
    pub whitespace: Vec<Token>,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} = {}", self.kind, self.value)
    }
}
