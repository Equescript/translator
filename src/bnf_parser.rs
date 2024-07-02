macro_rules! ConditionsCount {
    ($c: expr, $number: expr, $index: expr, [$($condition: expr),* $(,)?]) => {
        if $number == $index {
            ($( $c == $condition || )* false)
        } else {
            false
        }
    };
    ($c: expr, $number: expr, $index: expr, [$($condition: expr),* $(,)?] $($tail: tt)*) => {
        if $number == $index {
            ($( $c == $condition || )* false)
        } else {
            ConditionsCount!($c, $number, $index + 1usize, $($tail)*)
        }
    };
}
macro_rules! MatchConditions {
    ($c: expr, $number: expr) => {
        ConditionsCount!($c, $number, 0usize,
            ['0','1','2','3','4','5','6','7','8','9']
            ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']
            ['\u{0009}','\u{000A}','\u{000B}','\u{000C}','\u{000D}','\u{0020}','\u{0085}','\u{200E}','\u{200F}','\u{2028}','\u{2029}']
            ['\\','"','$','#','@','~','`','^','.']
            // ['\\','"','$','#','@','~','`','^','.',',',';',':','!','*','/','%','+'|'-'|'<'|'>'|'='|'&'|'|'|':'|'{'|'}'|'['|']'|'('|')']
        )
    };
}

mod Test {

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


pub struct Rules {

}

