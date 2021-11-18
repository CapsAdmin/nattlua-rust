use crate::syntax::Syntax;

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

pub fn runtime_syntax() -> Syntax {
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

pub fn typesystem_syntax() -> Syntax {
    let mut syntax = runtime_syntax();

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
