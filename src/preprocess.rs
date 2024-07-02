#[derive(Clone)]
pub enum Literal {
    Obj,
    Set,
    Map,
    Func,
    I32Number(String),
    UsizeNumber(String),
    F64Number(String),
    Character(String)
}

#[derive(Clone, Copy)]
pub enum Operator {
    Path, // ::
    Not, // !
    As, // as
    Mul, // *
    Div, // /
    Rem, // %
    Add, // +
    Sub, // -
    Eq, // ==
    NotEq, // !=
    LT, // < Less Than
    LE, // <= Less Than or Equal
    GT, // > Greater Than
    GE, // >= Greater Than or Equal
    In, // in
    And, // &&
    Or, // ||
    Assign, // =
    Separator, // |
}

#[derive(Clone, Copy)]
pub enum Bracket {
    LeftCurlyBrace, // {
    RightCurlyBrace, // }
    LeftSquareBracket, // [
    RightSquareBracket, // ]
    LeftParenthesis, // (
    RightParenthesis, // )
}

#[derive(Clone)]
pub enum SyntaxElement {
    Literal(Literal),
    Operator(Operator),
    Bracket(Bracket),
    Identifier(String),
}

fn check_invalid_char(c: char) -> Result<(), ()> {
    match c {
        '\\'|'"'|'$'|'#'|'@'|'~'|'`'|'^'|'.' => Err(()),
        _ => Ok(())
    }
}

fn is_invalid_identifier_char(c: char) -> bool {
    match c {
        '!'|'*'|'/'|'%'|'+'|'-'|'<'|'>'|'='|'&'|'|'|':'|'{'|'}'|'['|']'|'('|')'
        |'\\'|'"'|'$'|'#'|'@'|'~'|'`'|'^'|'.' => true,
        _ => false
    }
}

fn is_hex_char(c: char) -> bool {
    match c {
        '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F' => true,
        _ => false
    }
}

fn is_number_char(c: char) -> bool {
    match c {
        '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => true,
        _ => false
    }
}

enum StartCharType {
    None,
    Char([char; 12], usize),
    // Number(Vec<char>),
    Number(String, Option<String>, Option<String>),
    Symbol(char),
    Keyword([char; 5], usize),
    Identifier(String),
}

struct PatternMatch {
    elements: Vec<SyntaxElement>,
    start_char_type: StartCharType,
}

impl Default for PatternMatch {
    fn default() -> Self {
        Self { elements: Vec::new(), start_char_type: StartCharType::None }
    }
}

impl PatternMatch {
    fn judge_start_char_type(&mut self, c: char) -> Result<StartCharType, ()> {
        Ok(match c {
            '\u{0009}' | // U+0009 (horizontal tab, '\t')
            '\u{000A}' | // U+000A (line feed, '\n')
            '\u{000B}' | // U+000B (vertical tab)
            '\u{000C}' | // U+000C (form feed)
            '\u{000D}' | // U+000D (carriage return, '\r')
            '\u{0020}' | // U+0020 (space, ' ')
            '\u{0085}' | // U+0085 (next line)
            '\u{200E}' | // U+200E (left-to-right mark)
            '\u{200F}' | // U+200F (right-to-left mark)
            '\u{2028}' | // U+2028 (line separator)
            '\u{2029}' => StartCharType::None, // U+2029 (paragraph separator)
            '\'' => StartCharType::Char(['\'','\0','\0','\0','\0','\0','\0','\0','\0','\0','\0','\0'], 1),
            // '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => StartCharType::Number(vec![c]),
            '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => StartCharType::Number(c.to_string(), None, None),
            ':'|'!'|'<'|'>'|'='|'&'|'|'=> StartCharType::Symbol(c),
            // '*'|'/'|'%'|'+'|'-'|'{'|'}'|'['|']'|'('|')' => StartCharType::None,
            '*' => { self.elements.push(SyntaxElement::Operator(Operator::Mul)); StartCharType::None },
            '/' => { self.elements.push(SyntaxElement::Operator(Operator::Div)); StartCharType::None },
            '%' => { self.elements.push(SyntaxElement::Operator(Operator::Rem)); StartCharType::None },
            '+' => { self.elements.push(SyntaxElement::Operator(Operator::Add)); StartCharType::None },
            '-' => { self.elements.push(SyntaxElement::Operator(Operator::Sub)); StartCharType::None },
            '{' => { self.elements.push(SyntaxElement::Bracket(Bracket::LeftCurlyBrace)); StartCharType::None },
            '}' => { self.elements.push(SyntaxElement::Bracket(Bracket::RightCurlyBrace)); StartCharType::None },
            '[' => { self.elements.push(SyntaxElement::Bracket(Bracket::LeftSquareBracket)); StartCharType::None },
            ']' => { self.elements.push(SyntaxElement::Bracket(Bracket::RightSquareBracket)); StartCharType::None },
            '(' => { self.elements.push(SyntaxElement::Bracket(Bracket::LeftParenthesis)); StartCharType::None },
            ')' => { self.elements.push(SyntaxElement::Bracket(Bracket::RightParenthesis)); StartCharType::None },
            'a'|'f'|'i'|'m'|'o'|'s' => StartCharType::Keyword([c,'\0','\0','\0','\0'], 1),
            '\\'|'"'|'$'|'#'|'@'|'~'|'`'|'^'|'.' => return Err(()),
            _ => StartCharType::Identifier(c.to_string()),
        })
    }
    fn element_match_terminal(&mut self, element: SyntaxElement) {
        self.elements.push(element);
        self.start_char_type = StartCharType::None;
    }
    fn char_match(&mut self, c: char) -> Result<(), ()> {
        macro_rules! MatchTerminal {
            ($element: expr) => {
                {
                    self.elements.push($element);
                    self.start_char_type = StartCharType::None;
                }
            };
        }
        match &mut self.start_char_type {
            StartCharType::None => self.start_char_type = self.judge_start_char_type(c)?,
            StartCharType::Char(s, length) => {
                s[*length] = c;
                *length += 1;

                if length == &3 {
                    match (s[0], s[1], s[2]) {
                        ('\'', s1, '\'') => match s1 {
                            '\\' | '\'' => return Err(()),
                            _ => MatchTerminal!(SyntaxElement::Literal(Literal::Character({
                                let mut char_str = s[0].to_string();
                                char_str.push(s[1]); char_str.push(s[2]);
                                char_str
                            })))
                        },
                        _ => return Err(()),
                    }
                } else if length == &4 {
                    match (s[0], s[1], s[2], s[3]) {
                        ('\'', '\\', '0' | 'n' | 'r' | 't' | '\'', '\'') => {
                            MatchTerminal!(SyntaxElement::Literal(Literal::Character({
                                let mut char_str = s[0].to_string();
                                char_str.push(s[1]); char_str.push(s[2]); char_str.push(s[3]);
                                char_str
                            })))
                        },
                        ('\'', '\\', 'u', '{') => (),
                        _ => return Err(()),
                    }
                } else if length == &10 {
                    match (s[0], s[1], s[2], s[3], s[4], s[5], s[6], s[7], s[8], s[9]) {
                        ('\'', '\\', 'u', '{', s4, s5, s6, s7, s8, s9) => {
                            if (s8 == '}') && (s9 == '\'') &&
                                is_hex_char(s4) && is_hex_char(s5) && is_hex_char(s6) && is_hex_char(s7) {
                                MatchTerminal!(SyntaxElement::Literal(Literal::Character({
                                    let mut char_str = s[0].to_string();
                                    char_str.push(s[1]); char_str.push(s[2]); char_str.push(s[3]);
                                    char_str.push(s[4]); char_str.push(s[5]); char_str.push(s[6]);
                                    char_str.push(s[7]); char_str.push(s[8]); char_str.push(s[9]);
                                    char_str
                                })))
                            }
                        }
                        _ => return Err(())
                    }
                } else if length == &11 {
                    match (s[0], s[1], s[2], s[3], s[4], s[5], s[6], s[7], s[8], s[9], s[10]) {
                        ('\'', '\\', 'u', '{', s4, s5, s6, s7, s8, s9, s10) => {
                            if (s9 == '}') && (s10 == '\'') &&
                                is_hex_char(s4) && is_hex_char(s5) && is_hex_char(s6) && is_hex_char(s7) && is_hex_char(s8) {
                                MatchTerminal!(SyntaxElement::Literal(Literal::Character({
                                    let mut char_str = s[0].to_string();
                                    char_str.push(s[1]); char_str.push(s[2]); char_str.push(s[3]);
                                    char_str.push(s[4]); char_str.push(s[5]); char_str.push(s[6]);
                                    char_str.push(s[7]); char_str.push(s[8]); char_str.push(s[9]); char_str.push(s[10]);
                                    char_str
                                })))
                            }
                        }
                        _ => return Err(())
                    }
                } else if length == &12 {
                    match (s[0], s[1], s[2], s[3], s[4], s[5], s[6], s[7], s[8], s[9], s[10], s[11]) {
                        ('\'', '\\', 'u', '{', s4, s5, s6, s7, s8, s9, '}', '\'') => {
                            if is_hex_char(s4) && is_hex_char(s5) && is_hex_char(s6) && is_hex_char(s7) && is_hex_char(s8) && is_hex_char(s9) {
                                MatchTerminal!(SyntaxElement::Literal(Literal::Character({
                                    let mut char_str = s[0].to_string();
                                    char_str.push(s[1]); char_str.push(s[2]); char_str.push(s[3]);
                                    char_str.push(s[4]); char_str.push(s[5]); char_str.push(s[6]); char_str.push(s[7]);
                                    char_str.push(s[8]); char_str.push(s[9]); char_str.push(s[10]); char_str.push(s[11]);
                                    char_str
                                })))
                            }
                        }
                        _ => return Err(())
                    }
                }
            },
            StartCharType::Number(integer, decimal, suffix) => {
                match c {
                    '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => {
                        match suffix {
                            Some(s) => s.push(c),
                            None => match decimal {
                                Some(d) => d.push(c),
                                None => integer.push(c),
                            }
                        }
                    },
                    '.' => {
                        match decimal {
                            Some(_) => return Err(()), // 小数点不能出现两次
                            None => *decimal = Some(String::new()),
                        }
                    },
                    'f'|'u'|'s'|'i'|'z'|'e' => match suffix {
                        Some(s) => s.push(c),
                        None => *suffix = Some(c.to_string())
                    }
                    _ => return Err(()),
                }
                if let Some(s) = suffix {
                    if s == &"i32".to_string() {
                        match decimal {
                            Some(_) => return Err(()),
                            None => MatchTerminal!(SyntaxElement::Literal(Literal::I32Number({
                                let mut num_str = integer.clone();
                                num_str.push_str(s);
                                num_str
                            }))),
                        }
                    } else if s == &"f64".to_string() {
                        match decimal {
                            Some(d) => MatchTerminal!(SyntaxElement::Literal(Literal::F64Number({
                                let mut num_str = integer.clone();
                                num_str.push('.');
                                num_str.push_str(d);
                                num_str.push_str(s);
                                num_str
                            }))),
                            None => return Err(()),
                        }
                    } else if s == &"usize".to_string() {
                        match decimal {
                            Some(_) => return Err(()),
                            None => MatchTerminal!(SyntaxElement::Literal(Literal::UsizeNumber({
                                let mut num_str = integer.clone();
                                num_str.push_str(s);
                                num_str
                            }))),
                        }
                    } else {
                        if s.chars().count() > 5 {
                            return Err(());
                        }
                    }
                }
            },
            StartCharType::Symbol(s) => {
                // '!'|'<'|'>'|'='|'&'|'|'|':'
                match s {
                    ':' => match c {
                        ':' => MatchTerminal!(SyntaxElement::Operator(Operator::Path)),
                        _ => return Err(())// self.start_char_type = self.judge_start_char_type(c)?,
                    }
                    '!' => match c {
                        '=' => MatchTerminal!(SyntaxElement::Operator(Operator::NotEq)),
                        _ => { MatchTerminal!(SyntaxElement::Operator(Operator::Not)); self.start_char_type = self.judge_start_char_type(c)? },
                    }
                    '<' => match c {
                        '=' => MatchTerminal!(SyntaxElement::Operator(Operator::LE)),
                        _ => { MatchTerminal!(SyntaxElement::Operator(Operator::LT)); self.start_char_type = self.judge_start_char_type(c)? },
                    }
                    '>' => match c {
                        '=' => MatchTerminal!(SyntaxElement::Operator(Operator::GE)),
                        _ => { MatchTerminal!(SyntaxElement::Operator(Operator::GT)); self.start_char_type = self.judge_start_char_type(c)? },
                    }
                    '=' => match c {
                        '=' => MatchTerminal!(SyntaxElement::Operator(Operator::Eq)),
                        _ => { MatchTerminal!(SyntaxElement::Operator(Operator::Assign)); self.start_char_type = self.judge_start_char_type(c)? },
                    }
                    '&' => match c {
                        '&' => MatchTerminal!(SyntaxElement::Operator(Operator::And)),
                        _ => return Err(())// self.start_char_type = self.judge_start_char_type(c)?,
                    }
                    '|' => match c {
                        '|' => MatchTerminal!(SyntaxElement::Operator(Operator::Or)),
                        _ => { MatchTerminal!(SyntaxElement::Operator(Operator::Separator)); self.start_char_type = self.judge_start_char_type(c)? },
                    }
                    _ => return Err(()),
                }
            },
            StartCharType::Keyword(s, length) => {
                s[*length] = c;
                *length += 1;

                if length == &2 {

                    match (s[0], s[1]) {
                        ('a', 's') | ('i', 'n') | ('m', 'a') | ('o', 'b') | ('s', 'e') | ('f', 'u') => {},
                        _ => if is_invalid_identifier_char(s[1]) {
                            MatchTerminal!(SyntaxElement::Identifier(s[0].to_string()));
                            self.start_char_type = self.judge_start_char_type(c)?
                        } else {
                            self.start_char_type = StartCharType::Identifier(({
                                let mut identifier = s[0].to_string();
                                identifier.push(s[1]);
                                identifier
                            }))
                        }
                    }
                } else if length == &3 {
                    match (s[0], s[1], s[2]) {
                        ('a', 's', s2) => if is_invalid_identifier_char(s2) {
                            MatchTerminal!(SyntaxElement::Operator(Operator::As));
                            self.start_char_type = self.judge_start_char_type(c)?
                        } else {
                            self.start_char_type = StartCharType::Identifier(({
                                let mut identifier = s[0].to_string();
                                identifier.push(s[1]);
                                identifier.push(s[2]);
                                identifier
                            }))
                        },
                        ('i', 'n', s2) => if is_invalid_identifier_char(s2) {
                            MatchTerminal!(SyntaxElement::Operator(Operator::In));
                            self.start_char_type = self.judge_start_char_type(c)?
                        } else {
                            self.start_char_type = StartCharType::Identifier(({
                                let mut identifier = s[0].to_string();
                                identifier.push(s[1]);
                                identifier.push(s[2]);
                                identifier
                            }))
                        },
                        ('m', 'a', 'p') | ('o', 'b', 'j') | ('s', 'e', 't') | ('f', 'u', 'n')=> (),
                        _ => if is_invalid_identifier_char(s[2]) {
                            MatchTerminal!(SyntaxElement::Identifier({
                                let mut identifier = s[0].to_string();
                                identifier.push(s[1]);
                                identifier
                            }));
                            self.start_char_type = self.judge_start_char_type(c)?
                        } else {
                            self.start_char_type = StartCharType::Identifier(({
                                let mut identifier = s[0].to_string();
                                identifier.push(s[1]);
                                identifier.push(s[2]);
                                identifier
                            }))
                        },
                    }
                } else if length == &4 {
                    match (s[0], s[1], s[2], s[3]) {
                        ('m', 'a', 'p', s3) => if is_invalid_identifier_char(s3) {
                            MatchTerminal!(SyntaxElement::Literal(Literal::Map));
                            self.start_char_type = self.judge_start_char_type(c)?
                        } else {
                            self.start_char_type = StartCharType::Identifier(({
                                let mut identifier = s[0].to_string();
                                identifier.push(s[1]);
                                identifier.push(s[2]);
                                identifier.push(s[3]);
                                identifier
                            }))
                        },
                        ('o', 'b', 'j', s3) => if is_invalid_identifier_char(s3) {
                            MatchTerminal!(SyntaxElement::Literal(Literal::Obj));
                            self.start_char_type = self.judge_start_char_type(c)?
                        } else {
                            self.start_char_type = StartCharType::Identifier(({
                                let mut identifier = s[0].to_string();
                                identifier.push(s[1]);
                                identifier.push(s[2]);
                                identifier.push(s[3]);
                                identifier
                            }))
                        },
                        ('s', 'e', 't', s3) => if is_invalid_identifier_char(s3) {
                            MatchTerminal!(SyntaxElement::Literal(Literal::Set));
                            self.start_char_type = self.judge_start_char_type(c)?
                        } else {
                            self.start_char_type = StartCharType::Identifier(({
                                let mut identifier = s[0].to_string();
                                identifier.push(s[1]);
                                identifier.push(s[2]);
                                identifier.push(s[3]);
                                identifier
                            }))
                        },
                        ('f', 'u', 'n', 'c') => (),
                        _ => if is_invalid_identifier_char(s[3]) {
                            MatchTerminal!(SyntaxElement::Identifier({
                                let mut identifier = s[0].to_string();
                                identifier.push(s[1]);
                                identifier.push(s[2]);
                                identifier
                            }));
                            self.start_char_type = self.judge_start_char_type(c)?
                        } else {
                            self.start_char_type = StartCharType::Identifier(({
                                let mut identifier = s[0].to_string();
                                identifier.push(s[1]);
                                identifier.push(s[2]);
                                identifier.push(s[3]);
                                identifier
                            }))
                        },
                    }
                } else if length == &5 {
                    match (s[0], s[1], s[2], s[3], s[4]) {
                        ('f', 'u', 'n', 'c', s4) => if is_invalid_identifier_char(s4) {
                            MatchTerminal!(SyntaxElement::Literal(Literal::Map));
                            self.start_char_type = self.judge_start_char_type(c)?
                        } else {
                            self.start_char_type = StartCharType::Identifier(({
                                let mut identifier = s[0].to_string();
                                identifier.push(s[1]);
                                identifier.push(s[2]);
                                identifier.push(s[3]);
                                identifier.push(s[4]);
                                identifier
                            }))
                        },
                        _ => if is_invalid_identifier_char(s[4]) {
                            MatchTerminal!(SyntaxElement::Identifier({
                                let mut identifier = s[0].to_string();
                                identifier.push(s[1]);
                                identifier.push(s[2]);
                                identifier.push(s[3]);
                                identifier
                            }));
                            self.start_char_type = self.judge_start_char_type(c)?
                        } else {
                            self.start_char_type = StartCharType::Identifier(({
                                let mut identifier = s[0].to_string();
                                identifier.push(s[1]);
                                identifier.push(s[2]);
                                identifier.push(s[3]);
                                identifier.push(s[4]);
                                identifier
                            }))
                        },
                    }
                }
            },
            StartCharType::Identifier(s) => {
                if is_invalid_identifier_char(c) {
                    MatchTerminal!(SyntaxElement::Identifier(s.clone()));
                    self.start_char_type = self.judge_start_char_type(c)?
                } else {
                    s.push(c)
                }
            },
        };
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let a = 'a';
        assert!(&a == &'a');
        let s = "i32".to_string();
        let p = &s;
        assert!(p == &"i32".to_string())
    }
}
