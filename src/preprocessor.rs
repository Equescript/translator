use std::collections::VecDeque;

fn is_whitespace(c: char) -> bool {
    match c {
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
        '\u{2029}' => true, // U+2029 (paragraph separator)
        _ => false
    }
}

fn is_terminal(c: char) -> bool {
    match c {
        '\''|
        '!' |
        '*' |
        '/' |
        '%' |
        '+' |
        '-' |
        '<' |
        '>' |
        '=' |
        '&' |
        '|' |
        '{' |
        '}' |
        '[' |
        ']' |
        '(' |
        ')' => true,
        _ => is_whitespace(c)
    }
}

fn is_invalid(c: char) -> bool {
    match c {
        '\"' |
        '\\' => true,
        _ => false
    }
}

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

enum MatchResult {
    Finished(SyntaxElement, Option<char>),
    Unfinished(Buffer),
    Identifier(String),
}

fn char_match(c: char) -> Result<MatchResult, ()> {
    match c {
        '0' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::Number('0')))),
        '1' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::Number('1')))),
        '2' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::Number('2')))),
        '3' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::Number('3')))),
        '4' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::Number('4')))),
        '5' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::Number('5')))),
        '6' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::Number('6')))),
        '7' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::Number('7')))),
        '8' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::Number('8')))),
        '9' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::Number('9')))),
        '\'' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::CharacterStart))),
        'a' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::A))),
        'f' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::F))),
        'i' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::I))),
        'm' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::M))),
        'o' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::O))),
        's' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::S))),
        '!' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::Not))),
        '*' => Ok(MatchResult::Finished(SyntaxElement::Operator(Operator::Mul), None)),
        '/' => Ok(MatchResult::Finished(SyntaxElement::Operator(Operator::Div), None)),
        '%' => Ok(MatchResult::Finished(SyntaxElement::Operator(Operator::Rem), None)),
        '+' => Ok(MatchResult::Finished(SyntaxElement::Operator(Operator::Add), None)),
        '-' => Ok(MatchResult::Finished(SyntaxElement::Operator(Operator::Sub), None)),
        '<' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::LT))),
        '>' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::GT))),
        '=' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::Assign))),
        '&' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::And))),
        '|' => Ok(MatchResult::Unfinished(Buffer::One(FirstChar::Separator))),
        '{' => Ok(MatchResult::Finished(SyntaxElement::Bracket(Bracket::LeftCurlyBrace), None)),
        '}' => Ok(MatchResult::Finished(SyntaxElement::Bracket(Bracket::RightCurlyBrace), None)),
        '[' => Ok(MatchResult::Finished(SyntaxElement::Bracket(Bracket::LeftSquareBracket), None)),
        ']' => Ok(MatchResult::Finished(SyntaxElement::Bracket(Bracket::RightSquareBracket), None)),
        '(' => Ok(MatchResult::Finished(SyntaxElement::Bracket(Bracket::LeftParenthesis), None)),
        ')' => Ok(MatchResult::Finished(SyntaxElement::Bracket(Bracket::RightParenthesis), None)),
        _ => Ok(MatchResult::Identifier(c.to_string())),
    }
}

enum FirstChar {
    Number(char),
    CharacterStart,
    A,
    F,
    I,
    M,
    O,
    S,
    Not, // !
    LT, // <
    GT, // >
    Assign, // =
    And, // &
    Separator, // |
}

macro_rules! MatchUnfinished {
    ($c: expr, $char: expr, $element: expr) => {
        match $c {
            $char => Ok(MatchResult::Unfinished($element)),
            _ => Err(()),
        }
    };
}

macro_rules! MatchTerminal {
    ($c:expr, $element: expr) => {
        if is_terminal($c) {
            Ok(MatchResult::Finished($element, Some($c)))
        } else {
            Err(())
        }
    };
}

macro_rules! MatchIdentifierUnfinished {
    ($c: expr, $char: expr, $element: expr, $str: expr) => {
        match $c {
            $char => Ok(MatchResult::Unfinished($element)),
            _ => {
                MatchIdentifierTerminal!($c, SyntaxElement::Identifier($str.to_string()), $str)
            },
        }
    };
}

macro_rules! MatchIdentifierTerminal {
    ($c:expr, $element: expr, $s: expr) => {
        if is_terminal($c) {
            Ok(MatchResult::Finished($element, Some($c)))
        } else if is_invalid($c) {
            Err(())
        } else  {
            Ok(MatchResult::Identifier({let mut s = $s.to_string(); s.push($c); s}))
        }
    };
}

macro_rules! ConcatChar {
    ($s: expr, $c: expr) => {
        {let mut s = $s.to_string(); s.push($c); s}
    };
}

impl FirstChar {
    fn char_match(&self, c: char) -> Result<MatchResult, ()> {
        match self {
            FirstChar::Number(n) => match c {
                '0' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '1' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '2' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '3' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '4' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '5' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '6' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '7' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '8' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '9' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '.' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPoint(ConcatChar!(n, c))))),
                'f' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberF(ConcatChar!(n, c))))),
                'i' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberI(ConcatChar!(n, c))))),
                'u' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberU(ConcatChar!(n, c))))),
                _ => if is_whitespace(c) {
                    Ok(MatchResult::Finished(SyntaxElement::Literal(Literal::UsizeNumber(n.to_string())), None))
                } else {
                    Err(())
                }
            },
            FirstChar::CharacterStart => match c {
                '\\' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::EscapeCharacter))),
                _ => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NormalCharacter(c)))),
            },
            FirstChar::A => MatchIdentifierUnfinished!(c, 's', Buffer::Two(SecondChar::As), 'a'),
            FirstChar::F => MatchIdentifierUnfinished!(c, 'u', Buffer::Two(SecondChar::Fu), 'f'),
            FirstChar::I => MatchIdentifierUnfinished!(c, 'n', Buffer::Two(SecondChar::In), 'i'),
            FirstChar::M => MatchIdentifierUnfinished!(c, 'a', Buffer::Two(SecondChar::Ma), 'm'),
            FirstChar::O => MatchIdentifierUnfinished!(c, 'b', Buffer::Two(SecondChar::Ob), 'o'),
            FirstChar::S => MatchIdentifierUnfinished!(c, 'e', Buffer::Two(SecondChar::Se), 's'),
            FirstChar::Not => match c {
                '=' => Ok(MatchResult::Finished(SyntaxElement::Operator(Operator::NotEq), None)),
                _ => Ok(MatchResult::Finished(SyntaxElement::Operator(Operator::Not), Some(c)))
            },
            FirstChar::LT => match c {
                '=' => Ok(MatchResult::Finished(SyntaxElement::Operator(Operator::LE), None)),
                _ => Ok(MatchResult::Finished(SyntaxElement::Operator(Operator::LT), Some(c)))
            },
            FirstChar::GT => match c {
                '=' => Ok(MatchResult::Finished(SyntaxElement::Operator(Operator::GE), None)),
                _ => Ok(MatchResult::Finished(SyntaxElement::Operator(Operator::GT), Some(c)))
            },
            FirstChar::Assign => match c {
                '=' => Ok(MatchResult::Finished(SyntaxElement::Operator(Operator::Eq), None)),
                _ => Ok(MatchResult::Finished(SyntaxElement::Operator(Operator::Assign), Some(c)))
            },
            FirstChar::And => match c {
                '&' => Ok(MatchResult::Finished(SyntaxElement::Operator(Operator::And), None)),
                _ => Err(())
            },
            FirstChar::Separator => match c {
                '|' => Ok(MatchResult::Finished(SyntaxElement::Operator(Operator::Or), None)),
                _ => Ok(MatchResult::Finished(SyntaxElement::Operator(Operator::Separator), Some(c)))
            },
        }
    }
}

enum SecondChar {
    Number(String),
    NumberPoint(String),
    NumberPointNumber(String),
    NumberF(String),
    NumberI(String),
    NumberU(String),
    NormalCharacter(char),
    EscapeCharacter,
    As,
    Fu,
    In,
    Ma,
    Ob,
    Se,
}

impl SecondChar {
    fn char_match(&self, c: char) -> Result<MatchResult, ()> {
        match self {
            SecondChar::Number(n) => match c {
                '0' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '1' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '2' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '3' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '4' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '5' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '6' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '7' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '8' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '9' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::Number(ConcatChar!(n, c))))),
                '.' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPoint(ConcatChar!(n, c))))),
                'f' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberF(ConcatChar!(n, c))))),
                'i' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberI(ConcatChar!(n, c))))),
                'u' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberU(ConcatChar!(n, c))))),
                _ => if is_terminal(c) {
                    Ok(MatchResult::Finished(SyntaxElement::Literal(Literal::UsizeNumber(n.to_string())), Some(c)))
                } else {
                    Err(())
                }
            },
            SecondChar::NumberPoint(n) => match c {
                '0' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '1' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '2' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '3' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '4' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '5' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '6' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '7' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '8' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '9' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                _ => Err(())
            },
            SecondChar::NumberPointNumber(n) => match c {
                '0' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '1' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '2' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '3' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '4' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '5' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '6' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '7' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '8' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                '9' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberPointNumber(ConcatChar!(n, c))))),
                'f' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberF(ConcatChar!(n, c))))),
                'i' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberI(ConcatChar!(n, c))))),
                'u' => Ok(MatchResult::Unfinished(Buffer::Two(SecondChar::NumberU(ConcatChar!(n, c))))),
                _ => if is_terminal(c) {
                    Ok(MatchResult::Finished(SyntaxElement::Literal(Literal::UsizeNumber(n.to_string())), Some(c)))
                } else {
                    Err(())
                }
            },
            SecondChar::NumberF(n) => MatchUnfinished!(c, '6', Buffer::Three(ThirdChar::NumberF6(ConcatChar!(n, c)))),
            SecondChar::NumberI(n) => MatchUnfinished!(c, '3', Buffer::Three(ThirdChar::NumberI3(ConcatChar!(n, c)))),
            SecondChar::NumberU(n) => MatchUnfinished!(c, 's', Buffer::Three(ThirdChar::NumberUs(ConcatChar!(n, c)))),
            SecondChar::NormalCharacter(n) => match c {
                '\'' => Ok(MatchResult::Finished(SyntaxElement::Literal(Literal::Character({let mut s = '\''.to_string(); s.push(*n); s.push('\''); s})), None)),
                _ => Err(())
            },
            SecondChar::EscapeCharacter => match c {
                '0' | 'n' | 'r' | 't' | '\'' => Ok(MatchResult::Unfinished(Buffer::Three(ThirdChar::EscapeCharacter(c)))),
                'u' => Ok(MatchResult::Unfinished(Buffer::Three(ThirdChar::EscapeCharacterUnicodeStart))),
                _ => Err(())
            },
            SecondChar::As => MatchIdentifierTerminal!(c, SyntaxElement::Operator(Operator::As), "as"),
            SecondChar::Fu => MatchIdentifierUnfinished!(c, 'n', Buffer::Three(ThirdChar::Fun), "fu"),
            SecondChar::In => MatchIdentifierTerminal!(c, SyntaxElement::Operator(Operator::In), "in"),
            SecondChar::Ma => MatchIdentifierUnfinished!(c, 'p', Buffer::Three(ThirdChar::Map), "ma"),
            SecondChar::Ob => MatchIdentifierUnfinished!(c, 'j', Buffer::Three(ThirdChar::Obj), "ob"),
            SecondChar::Se => MatchIdentifierUnfinished!(c, 't', Buffer::Three(ThirdChar::Set), "se"),
        }
    }
}

enum ThirdChar {
    NumberF6(String),
    NumberI3(String),
    NumberUs(String),
    EscapeCharacter(char),
    EscapeCharacterUnicodeStart,
    Fun,
    Map,
    Obj,
    Set,
}

impl ThirdChar {
    fn char_match(&self, c: char) -> Result<MatchResult, ()> {
        match self {
            ThirdChar::NumberF6(n) => MatchUnfinished!(c, '4', Buffer::Four(FourthChar::NumberF64(ConcatChar!(n, c)))),
            ThirdChar::NumberI3(n) => MatchUnfinished!(c, '2', Buffer::Four(FourthChar::NumberI32(ConcatChar!(n, c)))),
            ThirdChar::NumberUs(n) => MatchUnfinished!(c, 'i', Buffer::Four(FourthChar::NumberUsi(ConcatChar!(n, c)))),
            ThirdChar::EscapeCharacter(n) => match c {
                '\'' => Ok(MatchResult::Finished(SyntaxElement::Literal(Literal::Character({let mut s = "\'\\".to_string(); s.push(*n); s.push('\''); s})), None)),
                _ => Err(())
            }
            ThirdChar::EscapeCharacterUnicodeStart => MatchUnfinished!(c, '{', Buffer::Four(FourthChar::EscapeCharacterUnicode("\'\\u{".to_string()))),
            ThirdChar::Fun => MatchIdentifierUnfinished!(c, 'c', Buffer::Four(FourthChar::Func), "fun"),
            ThirdChar::Map => MatchIdentifierTerminal!(c, SyntaxElement::Literal(Literal::Map), "map"),
            ThirdChar::Obj => MatchIdentifierTerminal!(c, SyntaxElement::Literal(Literal::Obj), "obj"),
            ThirdChar::Set => MatchIdentifierTerminal!(c, SyntaxElement::Literal(Literal::Set), "set"),
        }
    }
}

enum FourthChar {
    NumberF64(String),
    NumberI32(String),
    NumberUsi(String),
    EscapeCharacterUnicode(String),
    Func,
}

impl FourthChar {
    fn char_match(&self, c: char) -> Result<MatchResult, ()> {
        match self {
            FourthChar::NumberF64(n) => MatchTerminal!(c, SyntaxElement::Literal(Literal::F64Number(n.clone()))),
            FourthChar::NumberI32(n) => MatchTerminal!(c, SyntaxElement::Literal(Literal::I32Number(n.clone()))),
            FourthChar::NumberUsi(n) => MatchUnfinished!(c, 'z', Buffer::Five(FifthChar::NumberUsiz(ConcatChar!(n, c)))),
            FourthChar::EscapeCharacterUnicode(s) => match c {
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' => Ok(MatchResult::Unfinished(Buffer::Four(FourthChar::EscapeCharacterUnicode(ConcatChar!(s, c))))),
                '}' => Ok(MatchResult::Unfinished(Buffer::Five(FifthChar::EscapeCharacterUnicodeEnd(ConcatChar!(s, c))))),
                _ => Err(())
            }
            FourthChar::Func => MatchIdentifierTerminal!(c, SyntaxElement::Literal(Literal::Func), "func")
        }
    }
}

enum FifthChar {
    NumberUsiz(String),
    EscapeCharacterUnicodeEnd(String),
}

impl FifthChar {
    fn char_match(&self, c: char) -> Result<MatchResult, ()> {
        match self {
            FifthChar::NumberUsiz(n) => MatchUnfinished!(c, 'e', Buffer::Six(SixthChar::NumberUsize(ConcatChar!(n, c)))),
            FifthChar::EscapeCharacterUnicodeEnd(s) => match c {
                '\'' => Ok(MatchResult::Finished(SyntaxElement::Literal(Literal::Character(ConcatChar!(s, c))), None)),
                _ => Err(())
            }
        }
    }
}

enum SixthChar {
    NumberUsize(String),
}

impl SixthChar {
    fn char_match(&self, c: char) -> Result<MatchResult, ()> {
        match self {
            SixthChar::NumberUsize(n) => MatchTerminal!(c, SyntaxElement::Literal(Literal::UsizeNumber(n.clone())))
        }
    }
}

enum Buffer {
    None,
    One(FirstChar),
    Two(SecondChar),
    Three(ThirdChar),
    Four(FourthChar),
    Five(FifthChar),
    Six(SixthChar),
    Identifier(String)
}

impl Buffer {
    fn char_match(&mut self, c: char) -> Result<Option<(SyntaxElement, Option<char>)>, ()> {
        match match self {
            Buffer::None => char_match(c)?,
            Buffer::One(pattern) => pattern.char_match(c)?,
            Buffer::Two(pattern) => pattern.char_match(c)?,
            Buffer::Three(pattern) => pattern.char_match(c)?,
            Buffer::Four(pattern) => pattern.char_match(c)?,
            Buffer::Five(pattern) => pattern.char_match(c)?,
            Buffer::Six(pattern) => pattern.char_match(c)?,
            Buffer::Identifier(s) => {
                if is_invalid(c) {
                    return Err(());
                } else if is_terminal(c) {
                    // MatchResult::Finished(SyntaxElement::Identifier(s), Some(c))
                    return Ok(Some((SyntaxElement::Identifier(s.clone()), Some(c))));
                } else {
                    s.push(c);
                    return Ok(None);
                }
            }
        } {
            MatchResult::Finished(element, c) => { *self = Buffer::None; Ok(Some((element, c))) },
            MatchResult::Unfinished(buffer) => { *self = buffer; Ok(None) },
            MatchResult::Identifier(s) => { *self = Buffer::Identifier(s); Ok(None) },
        }
    }
}


fn pattern_match(s: String) -> Result<Vec<SyntaxElement>, ()> {
    let mut buffer = Buffer::None;
    let mut elements: Vec<SyntaxElement> = Vec::new();
    for c in s.chars() {
        let mut character = c;
        loop {
            match buffer.char_match(character)? {
                Some((element, option_c)) => {
                    elements.push(element);
                    match option_c {
                        Some(remain) => character = remain,
                        None => break,
                    }
                },
                None => break,
            }
        }
    }
    Ok(elements)
}

// '
// 0-9
// +-*/(){}[]
// abc
/*
'!'
'*'
'/'
'%'
'+'
'-'
'<'
'>'
'='
'&'
'|'
'{'
'}'
'['
']'
'('
')' */


