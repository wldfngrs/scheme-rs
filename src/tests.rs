#[cfg(test)]
mod lexer_tests {
    use crate::Lexer;
    use crate::TokenKind;
    use crate::Exactness;
    use crate::Radix;
    use std::collections::HashMap;

    fn generate_variable_test_data(hash_map: &mut HashMap<&str, TokenKind>) {
        hash_map.insert("list->vector", TokenKind::Variable);
        hash_map.insert("+", TokenKind::Variable);
        hash_map.insert("-", TokenKind::Variable);
        hash_map.insert("/", TokenKind::Variable);
        hash_map.insert("*", TokenKind::Variable);
        //hash_map.insert("...", TokenKind::Variable);
        hash_map.insert("<=?", TokenKind::Variable);
        hash_map.insert("number?", TokenKind::Variable);
        hash_map.insert("the-word-recursion-has-many-meanings", TokenKind::Variable);
        hash_map.insert("q", TokenKind::Variable);
        hash_map.insert("soup", TokenKind::Variable);
        hash_map.insert("V17a", TokenKind::Variable);
        hash_map.insert("a34kTMNs", TokenKind::Variable);
    }

    fn generate_keyword_test_data(hash_map: &mut HashMap<&str, TokenKind>) {
        hash_map.insert("else", TokenKind::Else);
        hash_map.insert("=>", TokenKind::Arrow);
        hash_map.insert("define", TokenKind::Define);
        hash_map.insert("unquote", TokenKind::Unquote);
        hash_map.insert("unquote-splicing", TokenKind::UnquoteSplicing);
        hash_map.insert("quote", TokenKind::Quote);
        hash_map.insert("lambda", TokenKind::Lambda);
        hash_map.insert("set!", TokenKind::SetExPt);
        hash_map.insert("begin", TokenKind::Begin);
        hash_map.insert("cond", TokenKind::Cond);
        hash_map.insert("and", TokenKind::And);
        hash_map.insert("or", TokenKind::Or);
        hash_map.insert("case", TokenKind::Case);
        hash_map.insert("let", TokenKind::Let);
        hash_map.insert("let*", TokenKind::LetStar);
        hash_map.insert("letrec", TokenKind::LetRec);
        hash_map.insert("do", TokenKind::Do);
        hash_map.insert("delay", TokenKind::Delay);
        hash_map.insert("quasiquote", TokenKind::Quasiquote);
    }

    fn generate_primary_test_data(hash_map: &mut HashMap<&str, TokenKind>) {
        hash_map.insert("(", TokenKind::Lparen);
        hash_map.insert(")", TokenKind::Rparen);
        hash_map.insert("#(", TokenKind::Sharplparen);
        hash_map.insert("#\\a", TokenKind::Character);
        hash_map.insert("#\\#", TokenKind::Character);
        hash_map.insert("#\\ ", TokenKind::Character);
        hash_map.insert("#\\\n", TokenKind::Character);
        hash_map.insert("#t", TokenKind::True);
        hash_map.insert("#f", TokenKind::False);
        hash_map.insert("'", TokenKind::Squote);
        hash_map.insert("`", TokenKind::Bquote);
        hash_map.insert(",", TokenKind::Comma);
        hash_map.insert(",@", TokenKind::Seqcomma);
    }

    fn generate_number_test_data(hash_map: &mut HashMap<&str, TokenKind>) {
        hash_map.insert("#b10#", TokenKind::Number(Exactness::Empty, Radix::Binary));
        hash_map.insert("#i#b10#", TokenKind::Number(Exactness::Inexact, Radix::Binary));
        hash_map.insert("#e#b10#", TokenKind::Number(Exactness::Exact, Radix::Binary));
        hash_map.insert("#o10", TokenKind::Number(Exactness::Empty, Radix::Octal));
        hash_map.insert("#o1234567", TokenKind::Number(Exactness::Empty, Radix::Octal));
        hash_map.insert("#d0123456789#", TokenKind::Number(Exactness::Empty, Radix::Decimal));
        hash_map.insert("0123456789##", TokenKind::Number(Exactness::Empty, Radix::Empty));
        hash_map.insert("#x10", TokenKind::Number(Exactness::Empty, Radix::Hexadecimal));
        hash_map.insert("#x70#", TokenKind::Number(Exactness::Empty, Radix::Hexadecimal));
        hash_map.insert("#x123456789abcdef", TokenKind::Number(Exactness::Empty, Radix::Hexadecimal));
        hash_map.insert("#xabcdef1234567", TokenKind::Number(Exactness::Empty, Radix::Hexadecimal));
        hash_map.insert("3+4i", TokenKind::Number(Exactness::Empty, Radix::Empty));
        hash_map.insert("3", TokenKind::Number(Exactness::Empty, Radix::Empty));
        hash_map.insert("-2.5+0.0i", TokenKind::Number(Exactness::Empty, Radix::Empty));
        hash_map.insert("#e1e10", TokenKind::Number(Exactness::Exact, Radix::Empty));
        hash_map.insert("6/10", TokenKind::Number(Exactness::Empty, Radix::Empty));
        hash_map.insert("6/3+.1i", TokenKind::Number(Exactness::Empty, Radix::Empty));
        hash_map.insert(".67##e12", TokenKind::Number(Exactness::Empty, Radix::Empty));
        hash_map.insert("12#.e10", TokenKind::Number(Exactness::Empty, Radix::Empty));
        hash_map.insert("12##.#e10", TokenKind::Number(Exactness::Empty, Radix::Empty));
        hash_map.insert("12.14#e10", TokenKind::Number(Exactness::Empty, Radix::Empty));
        hash_map.insert("3+0i", TokenKind::Number(Exactness::Empty, Radix::Empty));
        hash_map.insert("3.0", TokenKind::Number(Exactness::Empty, Radix::Empty));
        hash_map.insert("8/4", TokenKind::Number(Exactness::Empty, Radix::Empty));
        hash_map.insert("-8i", TokenKind::Number(Exactness::Empty, Radix::Empty));
        hash_map.insert("+12i", TokenKind::Number( Exactness::Empty, Radix::Empty));
        hash_map.insert("+i", TokenKind::Number( Exactness::Empty, Radix::Empty));
        hash_map.insert("-i", TokenKind::Number( Exactness::Empty, Radix::Empty));
    }

    fn generate_string_test_data(hash_map: &mut HashMap<&str, TokenKind>) {
        // r#"<string literal>"# represents a raw string literal,
        // allowing us to write strings without 'weird' escapes.
        hash_map.insert(r#""Hello, world!""#, TokenKind::String);
        hash_map.insert(r#""Line 1\nLine 2""#, TokenKind::String);
        hash_map.insert(r#""She said, \"Scheme is cool!\"""#, TokenKind::String);
        hash_map.insert(r#""Backslash: \\\ ""#, TokenKind::String);
        hash_map.insert(r#""Tab\tSeparated""#, TokenKind::String);
        hash_map.insert(r#""""#, TokenKind::String);                          
        hash_map.insert(r#""Newline\nTest""#, TokenKind::String);             
        hash_map.insert(r#""Quote: \"""#, TokenKind::String);                
        hash_map.insert(r#""Unicode: \u03A9""#, TokenKind::String);           
        hash_map.insert(r#""Multiple\\Backslashes\\Test""#, TokenKind::String);
        hash_map.insert(r#""Mix of escapes: \n\t\\\"""#, TokenKind::String);  
        hash_map.insert(r#""Ends with backslash\\""#, TokenKind::String);     
        hash_map.insert(r#""Embedded \"quotes\" inside""#, TokenKind::String);
    } 
    
    #[test]
    pub fn test_string_token_extraction() {
        let mut hash_map: HashMap<&str, TokenKind> = HashMap::new();
        generate_string_test_data(&mut hash_map);
        for (code, expected_token) in hash_map {
            let mut lexer = Lexer::new(code, code.chars());
            let returned_token = lexer.next_token().unwrap();
            assert_eq!(expected_token, returned_token.kind);
            assert_eq!(*code, lexer.code[returned_token.start..returned_token.start + returned_token.len]);
        }
    }

    #[test]
    pub fn test_variable_token_extraction() {
        let mut hash_map: HashMap<&str, TokenKind> = HashMap::new();
        generate_variable_test_data(&mut hash_map);
        for (code, expected_token) in hash_map {
            let mut lexer = Lexer::new(code, code.chars());
            let returned_token = lexer.next_token().unwrap();
            assert_eq!(expected_token, returned_token.kind);
            assert_eq!(*code, lexer.code[returned_token.start..returned_token.start + returned_token.len]);
        }
    }

    #[test]
    pub fn test_keyword_token_extraction() {
        let mut hash_map: HashMap<&str, TokenKind> = HashMap::new();
        generate_keyword_test_data(&mut hash_map);
        for (code, expected_token) in hash_map {
            let mut lexer = Lexer::new(code, code.chars());
            let returned_token = lexer.next_token().unwrap();
            assert_eq!(expected_token, returned_token.kind);
            assert_eq!(*code, lexer.code[returned_token.start..returned_token.start + returned_token.len]);
        }
    }

    #[test]
    pub fn test_primary_single_double_character_token_extraction() {
        let mut hash_map: HashMap<&str, TokenKind> = HashMap::new();
        generate_primary_test_data(&mut hash_map);
        for (code, expected_token) in hash_map {
            let mut lexer = Lexer::new(code, code.chars());
            let returned_token = lexer.next_token().unwrap();
            assert_eq!(expected_token, returned_token.kind);
            if expected_token != TokenKind::Character {
                assert_eq!(*code, lexer.code[returned_token.start..returned_token.start + returned_token.len]);
            } else {
                assert_eq!(code[returned_token.start..returned_token.start + returned_token.len], lexer.code[returned_token.start..returned_token.start + returned_token.len]);
            }
        }
    }

    #[test]
    pub fn test_number_token_extraction() {
        let mut hash_map: HashMap<&str, TokenKind> = HashMap::new();
        generate_number_test_data(&mut hash_map);
        for (code, expected_token) in hash_map {
            let mut lexer = Lexer::new(code, code.chars());
            let returned_token = lexer.next_token().unwrap();
            assert_eq!(expected_token, returned_token.kind);
            assert_eq!(*code, lexer.code[returned_token.start..returned_token.start + returned_token.len]);
        }
    }
}
