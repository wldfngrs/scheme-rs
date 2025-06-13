#[cfg(test)]
mod lexer_tests {
    use crate::Lexer;
    use crate::TokenKind;
    use std::collections::HashMap;

    fn generate_variable_test_data(hash_map: &mut HashMap<&str, TokenKind>) {
        hash_map.insert("list->vector", TokenKind::Variable);
        hash_map.insert("+", TokenKind::Variable);
        hash_map.insert("-", TokenKind::Variable);
        //hash_map.insert("...", TokenKind::Variable);
        hash_map.insert("<=?", TokenKind::Variable);
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
        hash_map.insert("'", TokenKind::Squote);
        hash_map.insert("`", TokenKind::Bquote);
        hash_map.insert(",", TokenKind::Comma);
        hash_map.insert(",@", TokenKind::Seqcomma);
        hash_map.insert(".", TokenKind::Point);
    } 

    #[test]
    pub fn test_variable_token_extraction() {
        let mut hash_map: HashMap<&str, TokenKind> = HashMap::new();
        generate_variable_test_data(&mut hash_map);
        for (code, expected_token) in hash_map {
            let mut lexer = Lexer::new(code, code.chars());
            assert_eq!(expected_token, lexer.next_token().unwrap().kind)
        }
    }

    #[test]
    pub fn test_keyword_token_extraction() {
        let mut hash_map: HashMap<&str, TokenKind> = HashMap::new();
        generate_keyword_test_data(&mut hash_map);
        for (code, expected_token) in hash_map {
            let mut lexer = Lexer::new(code, code.chars());
            assert_eq!(expected_token, lexer.next_token().unwrap().kind)
        }
    }

    #[test]
    pub fn test_primary_single_double_character_token_extraction() {
        let mut hash_map: HashMap<&str, TokenKind> = HashMap::new();
        generate_primary_test_data(&mut hash_map);
        for (code, expected_token) in hash_map {
            let mut lexer = Lexer::new(code, code.chars());
            assert_eq!(expected_token, lexer.next_token().unwrap().kind)
        }
    }
}