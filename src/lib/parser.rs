use core::panic;
use std::fmt::Binary;

use crate::{
    lua_syntax::{runtime_syntax, typesystem_syntax},
    syntax::Syntax,
    token::{Token, TokenType},
};

macro_rules! enum_struct {
    ($name:ident, $($x:ident),+) => {
        pub enum $name {
            $($x ($x)),+
        }
    }
}

pub struct EndOfFileStatement {
    tk_main: Token, // blank token with whitespace
}
pub struct AnalyzerDebugCodeStatement {
    lua_code: ValuePrimaryExpression,

    tk_main: Token, // §
}
pub enum Statement {
    EndOfFileStatement(EndOfFileStatement),
    AnalyzerDebugCodeStatement(AnalyzerDebugCodeStatement),
}

pub struct BaseTokensPrimaryExpression {
    tk_left_parenthesis: Vec<Token>,  // (
    tk_right_parenthesis: Vec<Token>, // )
    tk_type_as: Option<Token>,        // as
    standalone_letter: Box<Option<SubRuntimeExpression>>,
}

pub struct ValuePrimaryExpression {
    value: Token,

    tk_base: BaseTokensPrimaryExpression,
}

pub struct BinaryOperatorPrimaryExpression {
    operator: Token,
    left: Box<RuntimeExpression>,
    right: Box<RuntimeExpression>,

    tk_base: BaseTokensPrimaryExpression,
}

pub struct PrefixOperatorPrimaryExpression {
    operator: Token,
    right: Box<RuntimeExpression>,

    tk_base: BaseTokensPrimaryExpression,
}
pub struct PostfixOperatorPrimaryExpression {
    operator: Token,
    left: Box<RuntimeExpression>,

    tk_base: BaseTokensPrimaryExpression,
}

pub struct ReturnStatement {
    expressions: Vec<ValuePrimaryExpression>,

    tk_main: Token, // return
    tk_base: BaseTokensPrimaryExpression,
}

pub struct FunctionArgumentSubExpression {
    identifier: Option<Token>,
    type_expression: PrimaryRuntimeExpression,
    tk_type: Option<Token>,
}

pub struct FunctionReturnTypeSubExpression {
    tk_type: Option<Token>,
    identifier: Option<Token>,
    type_expression: PrimaryRuntimeExpression,
}

pub struct FunctionPrimaryExpression {
    arguments: Vec<FunctionArgumentSubExpression>,
    return_types: Vec<FunctionReturnTypeSubExpression>,
    statements: Vec<Statement>,

    tk_function: Token,              // function
    tk_arguments_left: Token,        // (
    tk_arguments_commas: Vec<Token>, // ,
    tk_arguments_right: Token,       // )
    tk_end: Token,                   // end
}

pub struct TableExpressionKeyValueSubExpression {
    expression_key: bool,
    key_expression: PrimaryRuntimeExpression,
    value_expression: PrimaryRuntimeExpression,

    tk_equal: Token, // =
    tk_left: Token,  // [
    tk_right: Token, // ]
}

pub struct TableSpreadSubExpression {
    expression: PrimaryRuntimeExpression,
    tk_spread: Token, // ...
}
pub struct TableKeyValueSubExpression {
    identifier: Token,
    value_expression: PrimaryRuntimeExpression,
    spread: Option<TableSpreadSubExpression>,

    tk_equal: Token, // =
}

pub struct TableIndexValueSubExpression {
    value_expression: PrimaryRuntimeExpression,
    spread: Option<TableSpreadSubExpression>,
    key: usize,
}

enum_struct!(
    TableSubExpression,
    TableExpressionKeyValueSubExpression,
    TableKeyValueSubExpression,
    TableIndexValueSubExpression
);

// { [key] = value, key = value, value }
pub struct TablePrimaryExpression {
    children: Vec<TableSubExpression>,

    spread: bool,
    is_array: bool,
    is_dictionary: bool,

    tk_table_left: Token,  // {
    tk_table_right: Token, // }
    tk_separators: Vec<Token>,

    tk_base: BaseTokensPrimaryExpression,
}

enum_struct!(
    PrimaryRuntimeExpression,
    ValuePrimaryExpression,
    BinaryOperatorPrimaryExpression,
    PrefixOperatorPrimaryExpression,
    PostfixOperatorPrimaryExpression,
    TablePrimaryExpression
);

pub enum RuntimeExpression {
    PrimaryRuntimeExpression(PrimaryRuntimeExpression),
    SubRuntimeExpression(SubRuntimeExpression),
}

trait BaseToken {
    fn get_base_tokens(&self) -> &BaseTokensPrimaryExpression;
}

impl BaseToken for PrimaryRuntimeExpression {
    fn get_base_tokens(&self) -> &BaseTokensPrimaryExpression {
        match self {
            PrimaryRuntimeExpression::ValuePrimaryExpression(x) => &x.tk_base,
            PrimaryRuntimeExpression::BinaryOperatorPrimaryExpression(x) => &x.tk_base,
            PrimaryRuntimeExpression::PrefixOperatorPrimaryExpression(x) => &x.tk_base,
            PrimaryRuntimeExpression::PostfixOperatorPrimaryExpression(x) => &x.tk_base,
            PrimaryRuntimeExpression::TablePrimaryExpression(x) => &x.tk_base,
        }
    }
}

enum_struct!(
    SubRuntimeExpression,
    TableSubExpression,
    TableIndexValueSubExpression,
    TableKeyValueSubExpression,
    TableExpressionKeyValueSubExpression,
    TableSpreadSubExpression
);

struct Parser<'a> {
    tokens: &'a Vec<Token>,
    position: usize,
    nodes: Vec<Statement>,

    runtime_syntax: &'a Syntax,
    typesystem_syntax: &'a Syntax,
}

impl Parser<'_> {
    fn get_token(&self, offset: usize) -> &Token {
        &self.tokens[self.position + offset]
    }

    fn len(&self) -> usize {
        self.tokens.len()
    }

    fn advance(&mut self, offset: usize) {
        self.position += offset;
    }

    fn is_type(&self, token_type: TokenType, offset: usize) -> bool {
        self.get_token(offset).kind == token_type
    }

    fn is_value(&self, value: &str, offset: usize) -> bool {
        self.get_token(offset).value == value
    }

    fn read_token(&mut self) -> Token {
        let tk = self.tokens[self.position].clone();
        self.advance(1);
        tk
    }

    fn expect_type(&mut self, token_type: TokenType) -> Option<Token> {
        if !self.is_type(token_type, 0) {
            return None;
        }

        Some(self.read_token())
    }

    fn expect_value(&mut self, value: &str) -> Token {
        if !self.is_value(value, 0) {
            panic!("Expected value '{}'", value);
        }

        self.read_token()
    }

    /////////////

    fn read_end_of_file(&mut self) -> Option<EndOfFileStatement> {
        if !self.is_type(TokenType::EndOfFile, 0) {
            return None;
        }

        let node = EndOfFileStatement {
            tk_main: self.read_token(),
        };

        Some(node)
    }

    fn read_parenthesis_expression(&mut self) -> Option<PrimaryRuntimeExpression> {
        if !self.is_value("(", 0) {
            return None;
        }

        let left_parenthesis = self.expect_value("(");
        if let Some(node) = self.read_primary_runtime_expression() {
            let right_parenthesis = self.expect_value(")");

            let base = node.get_base_tokens();
            // base.tk_left_parenthesis.push(left_parenthesis);
            //base.tk_right_parenthesis.push(right_parenthesis);

            return Some(node);
        } else {
            // error: empty parentheses group
        }
        None
    }

    fn read_prefix_operator_expression(&mut self) -> Option<PrimaryRuntimeExpression> {
        if !self.runtime_syntax.is_prefix_operator(self.get_token(0)) {
            return None;
        }

        Some(PrimaryRuntimeExpression::PrefixOperatorPrimaryExpression(
            PrefixOperatorPrimaryExpression {
                operator: self.read_token(),
                right: Box::new(self.read_expression(0).unwrap()),
                tk_base: BaseTokensPrimaryExpression {
                    tk_left_parenthesis: vec![],
                    tk_right_parenthesis: vec![],
                    tk_type_as: None,
                    standalone_letter: Box::new(None),
                },
            },
        ))
    }

    fn read_postfix_operator_expression(&mut self) -> Option<PostfixOperatorPrimaryExpression> {
        if !self.runtime_syntax.is_postfix_operator(self.get_token(0)) {
            return None;
        }

        Some(PostfixOperatorPrimaryExpression {
            left: Box::new(self.read_expression(0).unwrap()),
            operator: self.read_token(),
            tk_base: BaseTokensPrimaryExpression {
                tk_left_parenthesis: vec![],
                tk_right_parenthesis: vec![],
                tk_type_as: None,
                standalone_letter: Box::new(None),
            },
        })
    }

    fn read_binary_operator_expression(&mut self) -> Option<BinaryOperatorPrimaryExpression> {
        if !self.runtime_syntax.is_binary_operator(self.get_token(0)) {
            return None;
        }

        Some(BinaryOperatorPrimaryExpression {
            left: Box::new(self.read_expression(0).unwrap()),
            operator: self.read_token(),
            right: Box::new(self.read_expression(0).unwrap()),
            tk_base: BaseTokensPrimaryExpression {
                tk_left_parenthesis: vec![],
                tk_right_parenthesis: vec![],
                tk_type_as: None,
                standalone_letter: Box::new(None),
            },
        })
    }

    fn read_primary_runtime_expression(&mut self) -> Option<PrimaryRuntimeExpression> {
        self.read_parenthesis_expression()
            .or_else(|| self.read_prefix_operator_expression())
        //.or_else(|| self.read_postfix_operator_expression())
        //.or_else(|| self.read_binary_operator_expression())
    }

    fn read_sub_runtime_expression(&mut self) -> Option<SubRuntimeExpression> {
        //.or_else(|| self.read_prefix_operator_expression())
        //.or_else(|| self.read_postfix_operator_expression())
        //.or_else(|| self.read_binary_operator_expression())
        None
    }

    fn read_runtime_expression(&mut self, priority: u16) -> Option<RuntimeExpression> {
        //    if self.get_prefer_typesystem() {
        //return self.read_type_expression(priority);
        //}

        let mut node: Option<RuntimeExpression> = None;

        if let Some(primary) = self.read_primary_runtime_expression() {
            node = Some(RuntimeExpression::PrimaryRuntimeExpression(primary));

            if let Some(sub) = self.read_sub_runtime_expression() {
                /*if let PrimaryRuntimeExpression::ValuePrimaryExpression(primary_value) = primary {
                    if primary_value.value.kind == TokenType::Letter || primary_value.value.value == "..." {
                        primary_value.tk_base.standalone_letter = Box::new(Some(sub));
                    }
                }*/

                node = Some(RuntimeExpression::SubRuntimeExpression(sub));
            }
        }

        loop {
            let opt = self.runtime_syntax.get_operator_info(self.get_token(0));
            if opt.is_none() {
                break;
            }

            let info = opt.unwrap();

            if info.left_priority < priority {
                break;
            }

            let left_node = node;

            let s = BinaryOperatorPrimaryExpression {
                left: Box::new(left_node.unwrap()),
                operator: self.read_token(),
                right: Box::new(self.read_runtime_expression(info.right_priority).unwrap()),
                tk_base: BaseTokensPrimaryExpression {
                    tk_left_parenthesis: vec![],
                    tk_right_parenthesis: vec![],
                    tk_type_as: None,
                    standalone_letter: Box::new(None),
                },
            };

            node = Some(RuntimeExpression::PrimaryRuntimeExpression(
                PrimaryRuntimeExpression::BinaryOperatorPrimaryExpression(s),
            ));
        }

        node
    }

    fn read_value_expression(&mut self) -> Option<ValuePrimaryExpression> {
        if !self.runtime_syntax.is_token_value(self.get_token(0)) {
            return None;
        }

        Some(ValuePrimaryExpression {
            value: self.read_token(),
            tk_base: BaseTokensPrimaryExpression {
                tk_left_parenthesis: vec![],
                tk_right_parenthesis: vec![],
                tk_type_as: None,
                standalone_letter: Box::new(None),
            },
        })
    }

    fn read_expression(&mut self, priority: u16) -> Option<RuntimeExpression> {
        if self.runtime_syntax.is_token_value(self.get_token(0)) {
            return Some(RuntimeExpression::PrimaryRuntimeExpression(
                PrimaryRuntimeExpression::ValuePrimaryExpression(self.read_value_expression().unwrap()),
            ));
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        code::Code,
        lexer::Lexer,
        lua_syntax::{runtime_syntax, typesystem_syntax},
        parser::{Parser, PrimaryRuntimeExpression, RuntimeExpression},
    };

    #[test]
    fn parse() {
        let code = Code::new("-1337", "test");
        let mut lexer = Lexer::new(code);

        let (tokens, errors) = lexer.get_tokens();

        let mut parser = Parser {
            tokens: &tokens,
            position: 0,
            nodes: vec![],

            runtime_syntax: &runtime_syntax(),
            typesystem_syntax: &typesystem_syntax(),
        };

        let x = parser.read_runtime_expression(0).unwrap();
        if let RuntimeExpression::PrimaryRuntimeExpression(primary) = x {
            if let PrimaryRuntimeExpression::PrefixOperatorPrimaryExpression(value) = primary {
                assert_eq!(value.operator.value, "-");

                if let RuntimeExpression::PrimaryRuntimeExpression(primary) = value.right.as_ref() {
                    if let PrimaryRuntimeExpression::ValuePrimaryExpression(value) = primary {
                        assert_eq!(value.value.value, "1337");
                    }
                }
            }
        }

        let x = 0;
    }
}
