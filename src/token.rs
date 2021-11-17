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

#[derive(Clone)]
pub struct Token {
    pub kind: TokenType,
    pub value: String,
    pub start: usize,
    pub stop: usize,
    pub whitespace: Vec<Token>,
}
