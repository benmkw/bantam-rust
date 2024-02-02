use crate::bantam::TokenTy;

#[derive(Clone, Eq, PartialEq)]
pub struct Token {
    pub token_type: TokenTy,
    pub text: String,
}

/// A very primitive lexer. Takes a string and splits it into a series of
/// Tokens. Operators and punctuation are mapped to unique keywords. Names,
/// which can be any series of letters, are turned into NAME tokens. All other
/// characters are ignored (except to separate names). Numbers and strings are
/// not supported. This is really just the bare minimum to give the parser
/// something to work with.
pub struct Lexer {
    text: Vec<char>, // Using Vec<char> instead of String for simpler iteration.
    index: usize,
}

impl Lexer {
    /// Creates a new Lexer to tokenize the given string.
    /// * `text` - String to tokenize.
    pub fn new(text: &str) -> Self {
        Self {
            text: text.chars().collect(),
            index: 0,
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while self.index < self.text.len() {
            let c = self.text[self.index];
            self.index += 1;
            if let Some(token_type) = TokenTy::punctuators(c) {
                // Handle punctuation.
                return Some(Token {
                    token_type,
                    text: String::from(c),
                });
            } else if c.is_alphabetic() {
                // Handle names.
                let start = self.index - 1;
                while self.index < self.text.len() {
                    let c = self.text[self.index];
                    if !c.is_alphabetic() {
                        break;
                    }
                    self.index += 1;
                }
                let name: String = self.text[start..self.index].iter().collect();
                return Some(Token {
                    token_type: TokenTy::Name,
                    text: name,
                });
            } else {
                // Ignore all other characters (whitespace, etc.)
            }
        }

        // Once we've reached the end of the string, just return EOF tokens. We'll
        // just keeping returning them as many times as we're asked so that the
        // parser's lookahead doesn't have to worry about running out of tokens.
        Some(Token {
            token_type: TokenTy::Eof,
            text: String::new(),
        })
    }
}
