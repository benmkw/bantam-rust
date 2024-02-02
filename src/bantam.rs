pub mod lexer;
use lexer::Token;

///  Defines the different precedence levels used by the infix parsers. These
///  determine how a series of infix expressions will be grouped. For example,
///  "a + b * c - d" will be parsed as "(a + (b * c)) - d" because "*" has higher
///  precedence than "+" and "-". Here, bigger numbers mean higher precedence.
mod prec {
    pub const ASSIGNMENT: i32 = 1;
    pub const CONDITIONAL: i32 = 2;
    pub const SUM: i32 = 3;
    pub const PRODUCT: i32 = 4;
    pub const EXPONENT: i32 = 5;
    pub const PREFIX: i32 = 6;
    pub const POSTFIX: i32 = 7;
    pub const CALL: i32 = 8;
}

/// Generic infix parselet for a binary arithmetic operator. The only
/// difference when parsing, "+", "-", "*", "/", and "^" is precedence and
/// associativity, so we can
macro_rules! create_bin_op_parselet {
    ($assoc : expr, $prec : expr) => {
        // To handle right-associative operators like "^", we allow a slightly
        // lower precedence when parsing the right-hand side. This will let a
        // parselet with the same precedence appear on the right, which will then
        // take *this* parselet's result as its left-hand argument.
        |parser: &mut Parser<I>, left: Expr, token: Token| {
            let prec = $prec
                - match $assoc {
                    Assoc::Left => 0,
                    Assoc::Right => 1,
                };

            let right: Expr = parser.parse_expression_precedence(prec);

            Expr::Operator(Box::new(OperatorExpr {
                left,
                op: token.token_type,
                right,
            }))
        }
    };
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TokenTy {
    LeftParen,
    RightParen,
    Comma,
    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Caret,
    Tilde,
    Bang,
    Question,
    Colon,
    Name,
    Eof,
}

impl TokenTy {
    fn punctuator(&self) -> char {
        match *self {
            Self::LeftParen => '(',
            Self::RightParen => ')',
            Self::Comma => ',',
            Self::Assign => '=',
            Self::Plus => '+',
            Self::Minus => '-',
            Self::Asterisk => '*',
            Self::Slash => '/',
            Self::Caret => '^',
            Self::Tilde => '~',
            Self::Bang => '!',
            Self::Question => '?',
            Self::Colon => ':',
            _ => '\0',
        }
    }

    /// reverse of punctuator
    fn punctuators(c: char) -> Option<Self> {
        match c {
            '(' => return Some(Self::LeftParen),
            ')' => return Some(Self::RightParen),
            ',' => return Some(Self::Comma),
            '=' => return Some(Self::Assign),
            '+' => return Some(Self::Plus),
            '-' => return Some(Self::Minus),
            '*' => return Some(Self::Asterisk),
            '/' => return Some(Self::Slash),
            '^' => return Some(Self::Caret),
            '~' => return Some(Self::Tilde),
            '!' => return Some(Self::Bang),
            '?' => return Some(Self::Question),
            ':' => return Some(Self::Colon),
            _ => None,
        }
    }

    fn prefix_parselet<I: Iterator<Item = Token> + 'static>(
        &self,
    ) -> Option<&'static PrefixParselet<I>> {
        match self {
            Self::LeftParen => Some(&|parser: &mut Parser<I>, _token: Token| -> Expr {
                // Parses parentheses used to group an expression, like "a * (b + c)".
                let expression: Expr = parser.parse_expression();
                parser.consume_expected(Self::RightParen);
                expression
            }),
            Self::Plus | Self::Minus | Self::Tilde | Self::Bang => {
                Some(&|parser: &mut Parser<I>, token: Token| -> Expr {
                    // Generic prefix parselet for an unary arithmetic operator. Parses prefix
                    // unary "-", "+", "~", and "!" expressions.
                    //
                    // To handle right-associative operators like "^", we allow a slightly
                    // lower precedence when parsing the right-hand side. This will let a
                    // parselet with the same precedence appear on the right, which will then
                    // take *this* parselet's result as its left-hand argument.
                    let right: Expr = parser.parse_expression_precedence(prec::PREFIX);
                    Expr::Prefix(Box::new(PrefixExpr {
                        op: token.token_type,
                        right,
                    }))
                })
            }
            Self::Name => Some(&|_parser: &mut Parser<I>, token: Token| -> Expr {
                // Simple parselet for a named variable like "abc".
                Expr::Name(token.text)
            }),
            Self::RightParen
            | Self::Comma
            | Self::Assign
            | Self::Asterisk
            | Self::Slash
            | Self::Caret
            | Self::Question
            | Self::Colon
            | Self::Eof => None,
        }
    }

    fn infix_parselet<I: Iterator<Item = Token> + 'static>(
        &self,
    ) -> Option<(&'static InfixParselet<I>, i32)> {
        match self {
            Self::Plus | Self::Minus => {
                Some((&create_bin_op_parselet!(Assoc::Left, prec::SUM), prec::SUM))
            }
            Self::Asterisk | Self::Slash => Some((
                &create_bin_op_parselet!(Assoc::Left, prec::PRODUCT),
                prec::PRODUCT,
            )),
            Self::Caret => Some((
                &create_bin_op_parselet!(Assoc::Right, prec::EXPONENT),
                prec::EXPONENT,
            )),
            // For kicks, we'll make "!" both prefix and postfix, kind of like ++.
            // bp.postfix(TokenType::Bang, precedence::POSTFIX);
            Self::Bang => Some((
                &|_parser: &mut Parser<I>, left: Expr, token: Token| -> Expr {
                    // Generic infix parselet for an unary arithmetic operator. Parses postfix
                    // unary "?" expressions.
                    Expr::Postfix(Box::new(PostfixExpr {
                        left,
                        op: token.token_type,
                    }))
                },
                prec::POSTFIX,
            )),

            Self::LeftParen => Some((
                &|parser: &mut Parser<I>, left: Expr, _token: Token| -> Expr {
                    // Parselet to parse a function call like "a(b, c, d)".
                    // Parse the comma-separated arguments until we hit, ")".
                    let mut args: Vec<Expr> = vec![];

                    // There may be no arguments at all.
                    if !parser.match_token(Self::RightParen) {
                        loop {
                            args.push(parser.parse_expression());

                            if !parser.match_token(Self::Comma) {
                                break;
                            }
                        }
                        parser.consume_expected(Self::RightParen);
                    }

                    Expr::Call(Box::new(CallExpr { fn_: left, args }))
                },
                prec::CALL,
            )),
            Self::Assign => Some((
                &|parser: &mut Parser<I>, left: Expr, _token: Token| -> Expr {
                    // Parses assignment expressions like "a = b". The left side of an assignment
                    // expression must be a simple name like "a", and expressions are
                    // right-associative. (In other words, "a = b = c" is parsed as "a = (b = c)").
                    let right: Expr = parser.parse_expression_precedence(prec::ASSIGNMENT - 1);

                    let left_name_expr = if let Expr::Name(name) = left {
                        name
                    } else {
                        panic!("The left-hand side of an assignment must be a name.")
                    };

                    Expr::Assign(Box::new(AssignExpr {
                        name: left_name_expr,
                        right,
                    }))
                },
                prec::ASSIGNMENT,
            )),
            Self::Question => Some((
                &|parser: &mut Parser<I>, left: Expr, _token: Token| -> Expr {
                    // Parselet for the condition or "ternary" operator, like "a ? b : c".
                    let then_arm: Expr = parser.parse_expression();
                    parser.consume_expected(Self::Colon);
                    let else_arm: Expr = parser.parse_expression_precedence(prec::CONDITIONAL - 1);

                    Expr::Conditional(Box::new(ConditionalExpr {
                        cond: Box::new(left),
                        then: Box::new(then_arm),
                        else_: Box::new(else_arm),
                    }))
                },
                prec::CONDITIONAL,
            )),
            Self::RightParen | Self::Comma | Self::Tilde | Self::Colon | Self::Name | Self::Eof => {
                None
            }
        }
    }
}

pub struct Parser<I: Iterator<Item = Token> + 'static> {
    tokens: I,
    read: Vec<Token>,
}

impl<I: Iterator<Item = Token> + 'static> Parser<I> {
    pub fn new(tokens: I) -> Self {
        Self {
            tokens,
            read: vec![],
        }
    }

    fn parse_expression_precedence(&mut self, precedence: i32) -> Expr {
        let mut token: Token = self.consume();

        let prefix_opt: Option<&PrefixParselet<I>> = token.token_type.prefix_parselet();
        assert!(prefix_opt.is_some(), "Could not parse \"{}\".", &token.text);
        let prefix: &PrefixParselet<I> = prefix_opt.unwrap();

        let mut left = prefix(self, token);

        let get_precedence = |selv: &mut Self| -> i32 {
            let token_type = selv.look_ahead(0).token_type;
            let parser: Option<(&InfixParselet<I>, i32)> = token_type.infix_parselet();

            parser.map_or(0, |(_parser, precedence)| precedence)
        };

        while precedence < get_precedence(self) {
            token = self.consume();

            let infix: &InfixParselet<I> = token.token_type.infix_parselet().unwrap().0;
            left = infix(self, left, token);
        }

        left
    }

    pub fn parse_expression(&mut self) -> Expr {
        self.parse_expression_precedence(0)
    }

    fn match_token(&mut self, expected: TokenTy) -> bool {
        let token = self.look_ahead(0);
        if token.token_type != expected {
            return false;
        }

        self.consume();
        true
    }

    fn consume_expected(&mut self, expected: TokenTy) -> Token {
        let token = self.look_ahead(0);
        assert!(
            token.token_type == expected,
            "Expected token {expected:?} and found {:?}",
            token.token_type
        );

        self.consume()
    }

    fn consume(&mut self) -> Token {
        // Make sure we've read the token.
        self.look_ahead(0);
        self.read.remove(0)
    }

    fn look_ahead(&mut self, distance: usize) -> Token {
        // Read in as many as needed.
        while distance >= self.read.len() {
            self.read.push(self.tokens.next().unwrap());
        }

        // Get the queued token.
        self.read[distance].clone()
    }
}

/// A prefix unary arithmetic expression like "!a" or "-b".
pub struct PrefixExpr {
    pub op: TokenTy,
    pub right: Expr,
}

/// A postfix unary arithmetic expression like "a!".
pub struct PostfixExpr {
    pub left: Expr,
    pub op: TokenTy,
}

/// A binary arithmetic expression like "a + b" or "c ^ d".
pub struct OperatorExpr {
    pub left: Expr,
    pub op: TokenTy,
    pub right: Expr,
}

pub enum Expr {
    Conditional(Box<ConditionalExpr>),
    Call(Box<CallExpr>),
    Assign(Box<AssignExpr>),
    Prefix(Box<PrefixExpr>),
    Postfix(Box<PostfixExpr>),
    Operator(Box<OperatorExpr>),
    /// A simple variable name expression like "abc".
    Name(String),
}

impl Expr {
    pub fn print(&self) -> String {
        let mut res = String::new();
        self.write(&mut res).unwrap();
        res
    }

    fn write<W: core::fmt::Write>(&self, out: &mut W) -> core::fmt::Result {
        match self {
            Self::Conditional(e) => {
                out.write_char('(')?;
                e.cond.write(out)?;
                out.write_str(" ? ")?;
                e.then.write(out)?;
                out.write_str(" : ")?;
                e.else_.write(out)?;
                out.write_char(')')?;
            }
            Self::Call(e) => {
                e.fn_.write(out)?;
                out.write_char('(')?;
                for (i, arg) in e.args.iter().enumerate() {
                    arg.write(out)?;
                    if i + 1 < e.args.len() {
                        out.write_str(", ")?;
                    }
                }
                out.write_char(')')?;
            }
            Self::Assign(e) => {
                out.write_char('(')?;
                out.write_str(&e.name)?;
                out.write_str(" = ")?;
                e.right.write(out)?;
                out.write_char(')')?;
            }
            Self::Prefix(e) => {
                out.write_char('(')?;
                out.write_char(e.op.punctuator())?;
                e.right.write(out)?;
                out.write_char(')')?;
            }
            Self::Postfix(e) => {
                out.write_char('(')?;
                e.left.write(out)?;
                out.write_char(e.op.punctuator())?;
                out.write_char(')')?;
            }
            Self::Operator(e) => {
                out.write_char('(')?;
                e.left.write(out)?;
                out.write_char(' ')?;
                out.write_char(e.op.punctuator())?;
                out.write_char(' ')?;
                e.right.write(out)?;
                out.write_char(')')?;
            }
            Self::Name(name) => {
                out.write_str(name)?;
            }
        }
        Ok(())
    }
}

/// A ternary conditional expression like "a ? b : c".
pub struct ConditionalExpr {
    pub cond: Box<Expr>,
    pub then: Box<Expr>,
    pub else_: Box<Expr>,
}

/// A function call like "a(b, c, d)".
pub struct CallExpr {
    fn_: Expr,
    pub args: Vec<Expr>,
}

/// An assignment expression like "a = b".
pub struct AssignExpr {
    pub name: String,
    pub right: Expr,
}

/// One of the two interfaces used by the Pratt parser. A `PrefixParselet` is
/// associated with a token that appears at the beginning of an expression. It
/// will be called with the consumed leading token, and the
/// parselet is responsible for parsing anything that comes after that token.
/// This interface is also used for single-token expressions like variables, in
/// which case `parse()` simply doesn't consume any more tokens.
type PrefixParselet<I> = dyn Fn(&mut Parser<I>, Token) -> Expr;

/// One of the two parselet interfaces used by the Pratt parser. An
/// `InfixParselet` is associated with a token that appears in the middle of the
/// expression it parses. Its `parse()` method will be called after the left-hand
/// side has been parsed, and it in turn is responsible for parsing everything
/// that comes after the token. This is also used for postfix expressions, in
/// which case it simply doesn't consume any more tokens in its `parse()` call.
type InfixParselet<I> = dyn Fn(&mut Parser<I>, Expr, Token) -> Expr;

#[derive(Debug, Clone, Copy)]
pub enum Assoc {
    Left,
    Right,
}
