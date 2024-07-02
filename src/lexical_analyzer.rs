// use {IA, RI, RuleInstruction, RICall, NUMBER, HEX_NUMBER, WHITESPACE, INVALID_CHAR, INVALID_IDENTIFIER_CHAR, MATCH_TERMINAL, NEXT};
use crate::pattern_matcher::{IA, RuleInstruction, ImplRulesInto, PatternMatcher, TokenLocation, MATCH_TERMINAL, NEXT, Token, ConditionMatch};
const NUMBER: &str = "0123456789";
const HEX_NUMBER: &str = "0123456789ABCDEFabcdef";
const WHITESPACE: &str = "\u{0009}\u{000A}\u{000B}\u{000C}\u{000D}\u{0020}\u{0085}\u{200E}\u{200F}\u{2028}\u{2029}";
const INVALID_CHAR: &str = "\\\"$#@~`^./";
const INVALID_IDENTIFIER_CHAR: &str = "\\\"$#@~`^./,;:!*/%+-<>=&|{}[]()\u{0009}\u{000A}\u{000B}\u{000C}\u{000D}\u{0020}\u{0085}\u{200E}\u{200F}\u{2028}\u{2029}";
const INVALID_IDENTIFIER_CHAR_AND_NUMBER: &str = "0123456789\\\"$#@~`^./,;:!*/%+-<>=&|{}[]()\u{0009}\u{000A}\u{000B}\u{000C}\u{000D}\u{0020}\u{0085}\u{200E}\u{200F}\u{2028}\u{2029}";

type RI = RuleInstruction<char, LexicalRules, &'static str>;

macro_rules! DefaultCall {
    (($condition: expr, $call: ident, $address: expr)) => {
        (Some($condition), LexicalRules::$call, $address)
    };
    (($call: ident, $address: expr)) => {
        (None, LexicalRules::$call, $address)
    };
    ($call: ident) => {
        (None, LexicalRules::$call, NEXT)
    };
}

macro_rules! RICall {
    ([ $($call_instruction: tt),* $(,)? ]) => {
        RI::Call(&[
            $(DefaultCall!($call_instruction)),*
        ], 0usize $(+ {$call_instruction; 1usize} )*)
    };
    ($call: ident, $address: expr) => {
        RI::Call(&[
            (None, LexicalRules::$call, $address)
        ], 1usize)
    };
    ($call_instruction: ident) => {
        RI::Call(&[
            (None, LexicalRules::$call_instruction, NEXT)
        ], 1usize)
    };
}

const w: [RI; 25] = [
    RI::IsGoto(WHITESPACE, IA::Relative(4)),
    RI::IsGoto("/", IA::Relative(2)),
    RI::Is(""), // 第一个不是空格也不是 / 就错误结束
    RI::NotGoto(WHITESPACE, IA::Relative(3)),
    RI::Not(""), // 占位匹配空格
    RI::NotGoto("", IA::Relative(-2)),
    RI::IsGoto("/", IA::Relative(2)),
    RI::ErrEnd(None), // 之后的结束需要返回上级重新匹配最后的字符
    RI::Not(""), // 占位匹配 /
    RI::IsGoto("/", IA::Relative(10)),
    RI::ErrEnd(Some(LexicalRules::div_operator)),
    RI::Is("*"),
    RI::IsGoto("*", IA::Relative(3)),
    RI::Not(""),
    RI::NotGoto("", IA::Relative(-2)),
    RI::Not(""), // 占位匹配 *
    RI::NotGoto("/", IA::Relative(-4)), // 下一个不是 / 就返回正常的匹配
    RI::Not(""), // 占位匹配 /
    RI::NotGoto("", IA::Absolute(3)), // 块注释结束，返回空格匹配
    RI::Not(""), // 占位匹配 /
    RI::IsGoto("\n", IA::Relative(3)), // 换行代表行注释结束
    RI::Not(""), // 匹配任何字符
    RI::NotGoto("", IA::Relative(-2)),
    RI::Not(""), // 占位匹配 \n
    RI::NotGoto("", IA::Absolute(3)), // 行注释结束，返回空格匹配
];

const number_str: [RI; 5] = [
    RI::Is(NUMBER),
    RI::NotGoto(NUMBER, IA::Relative(3)),
    RI::Not(""),
    RI::NotGoto("", IA::Relative(-2)),
    RI::ErrEnd(None),
];

const hex_str: [RI; 5] = [
    RI::Is(HEX_NUMBER),
    RI::NotGoto(HEX_NUMBER, IA::Relative(3)),
    RI::Not(""),
    RI::NotGoto("", IA::Relative(-2)),
    RI::ErrEnd(None),
];

const char_literal: [RI; 16] = [
    RI::Is("'"),
    RI::IsGoto("\\", IA::Relative(4)),
    RI::Not("'"), // any char except ' \
    RI::IsGoto("'", MATCH_TERMINAL),
    RI::Is(""),
    RI::Not(""), // \
    RI::IsGoto("u", IA::Relative(4)),
    RI::Is("0nrt'\\"),
    RI::IsGoto("'", MATCH_TERMINAL),
    RI::Is(""),
    RI::Not(""), // u
    RI::Is("{"),
    RICall!(hex_str),
    RI::Is("}"),
    RI::IsGoto("'", MATCH_TERMINAL),
    RI::Is(""),
];

const i32_literal: [RI; 5] = [
    RICall!(number_str),
    RI::Is("i"),
    RI::Is("3"),
    RI::IsGoto("2", MATCH_TERMINAL),
    RI::Is(""),
];

const usize_literal: [RI; 7] = [
    RICall!(number_str),
    RI::Is("u"),
    RI::Is("s"),
    RI::Is("i"),
    RI::Is("z"),
    RI::IsGoto("e", MATCH_TERMINAL),
    RI::Is(""),
];

const f64_literal: [RI; 8] = [
    RICall!(number_str),
    RI::IsGoto("f", IA::Relative(3)),
    RI::Is("."),
    RICall!(number_str),
    RI::Is("f"),
    RI::Is("6"),
    RI::IsGoto("4", MATCH_TERMINAL),
    RI::Is(""),
];

const not_operator: [RI; 1] = [RI::Is("")];
const add_operator: [RI; 2] = [RI::IsGoto("+", MATCH_TERMINAL), RI::Is("")];
const sub_operator: [RI; 1] = [RI::Is("")];
const arrow_operator: [RI; 3] = [
    RI::Is("-"),
    RI::IsGoto(">", MATCH_TERMINAL),
    RI::ErrEnd(Some(LexicalRules::sub_operator)),
];
const mul_operator: [RI; 2] = [RI::IsGoto("*", MATCH_TERMINAL), RI::Is("")];
// const div_operator: [RI; 2] = [RI::IsGoto("/", MATCH_TERMINAL), RI::Is("")];
const div_operator: [RI; 1] = [RI::Is("")];
const rem_operator: [RI; 2] = [RI::IsGoto("%", MATCH_TERMINAL), RI::Is("")];
const assign_operator: [RI; 1] = [RI::Is("")];
const eq_operator: [RI; 3] = [
    RI::Is("="),
    RI::IsGoto("=", MATCH_TERMINAL),
    RI::ErrEnd(Some(LexicalRules::assign_operator)),
];
const noteq_operator: [RI; 3] = [
    RI::Is("!"),
    RI::IsGoto("=", MATCH_TERMINAL),
    RI::ErrEnd(Some(LexicalRules::not_operator)),
];
const lt_operator: [RI; 1] = [RI::Is("")];
const le_operator: [RI; 3] = [
    RI::Is("<"),
    RI::IsGoto("=", MATCH_TERMINAL),
    RI::ErrEnd(Some(LexicalRules::lt_operator)),
];
const gt_operator: [RI; 1] = [RI::Is("")];
const ge_operator: [RI; 3] = [
    RI::Is(">"),
    RI::IsGoto("=", MATCH_TERMINAL),
    RI::ErrEnd(Some(LexicalRules::gt_operator)),
];
const and_operator: [RI; 3] = [
    RI::Is("&"),
    RI::IsGoto("&", MATCH_TERMINAL),
    RI::Is(""),
];
const separator: [RI; 1] = [RI::Is("")];
const or_operator: [RI; 3] = [
    RI::Is("|"),
    RI::IsGoto("|", MATCH_TERMINAL),
    RI::ErrEnd(Some(LexicalRules::separator)),
];
const path_connector: [RI; 3] = [
    RI::Is(":"),
    RI::IsGoto(":", MATCH_TERMINAL),
    RI::Is(""),
];
const comma: [RI; 2] = [RI::IsGoto(",", MATCH_TERMINAL), RI::Is("")];
const statement_end: [RI; 2] = [RI::IsGoto(";", MATCH_TERMINAL), RI::Is("")];
const left_curly_brace: [RI; 2] = [RI::IsGoto("{", MATCH_TERMINAL), RI::Is("")];
const right_curly_brace: [RI; 2] = [RI::IsGoto("}", MATCH_TERMINAL), RI::Is("")];
const left_square_bracket: [RI; 2] = [RI::IsGoto("[", MATCH_TERMINAL), RI::Is("")];
const right_square_bracket: [RI; 2] = [RI::IsGoto("]", MATCH_TERMINAL), RI::Is("")];
const left_parenthesis: [RI; 2] = [RI::IsGoto("(", MATCH_TERMINAL), RI::Is("")];
const right_parenthesis: [RI; 2] = [RI::IsGoto(")", MATCH_TERMINAL), RI::Is("")];

/* const identifier: [RI; 6] = [
    RI::DebugIsGoto(NUMBER, IA::Relative(5), "identifier 0"),
    RI::DebugIsGoto(INVALID_IDENTIFIER_CHAR, IA::Relative(4), "identifier 1"),
    RI::DebugNot("", "identifier 2"),
    RI::DebugNotGoto(INVALID_IDENTIFIER_CHAR, IA::Relative(-1), "identifier 3"),
    RI::DebugErrEnd("identifier 4"),
    RI::DebugIs("", "identifier 5"),
]; */

const identifier: [RI; 6] = [
    RI::IsGoto(NUMBER, IA::Relative(5)),
    RI::IsGoto(INVALID_IDENTIFIER_CHAR, IA::Relative(4)),
    RI::Not(""),
    RI::NotGoto(INVALID_IDENTIFIER_CHAR, IA::Relative(-1)),
    RI::ErrEnd(None),
    RI::Is(""),
];
const JUMP: IA = IA::Relative(1);
const any: [RI; 1] = [
/*     RI::IsGoto("+", JUMP),
    RI::IsGoto("*", JUMP),
    RI::IsGoto("%", JUMP),
    RI::IsGoto(",", JUMP),
    RI::IsGoto(";", JUMP),
    RI::IsGoto("{", JUMP),
    RI::IsGoto("}", JUMP),
    RI::IsGoto("[", JUMP),
    RI::IsGoto("]", JUMP),
    RI::IsGoto("(", JUMP),
    RI::IsGoto(")", JUMP), */
    RICall!([
        (w, IA::Absolute(0)), // /
        (("'", true), char_literal, IA::Absolute(0)),
        ((NUMBER, true), i32_literal, IA::Absolute(0)),
        ((NUMBER, true), usize_literal, IA::Absolute(0)),
        ((NUMBER, true), f64_literal, IA::Absolute(0)),
        ((INVALID_IDENTIFIER_CHAR_AND_NUMBER, false), identifier, IA::Absolute(0)),
        (("+", true), add_operator, IA::Absolute(0)), // +
        (("*", true), mul_operator, IA::Absolute(0)), // *
        (("%", true), rem_operator, IA::Absolute(0)), // %
        (("=", true), eq_operator, IA::Absolute(0)), // == =
        (("!", true), noteq_operator, IA::Absolute(0)), // != !
        (("<", true), le_operator, IA::Absolute(0)), // <= <
        ((">", true), ge_operator, IA::Absolute(0)), // >= >
        (("&", true), and_operator, IA::Absolute(0)), // &&
        (("|", true), or_operator, IA::Absolute(0)), // || |
        ((":", true), path_connector, IA::Absolute(0)), // ::
        ((",", true), comma, IA::Absolute(0)), // ,
        (("-", true), arrow_operator, IA::Absolute(0)), // -> -
        ((";", true), statement_end, IA::Absolute(0)), // ;
        (("{", true), left_curly_brace, IA::Absolute(0)), // {
        (("}", true), right_curly_brace, IA::Absolute(0)), // }
        (("[", true), left_square_bracket, IA::Absolute(0)), // [
        (("]", true), right_square_bracket, IA::Absolute(0)), // ]
        (("(", true), left_parenthesis, IA::Absolute(0)), // (
        ((")", true), right_parenthesis, IA::Absolute(0)), // )
    ]),
];

ImplRulesInto!{
#[derive(Clone, Copy, Debug)]
pub enum LexicalRules {
    w,
    number_str,
    hex_str,

    char_literal,
    i32_literal,
    usize_literal,
    f64_literal,

    not_operator,
    add_operator,
    sub_operator,
    mul_operator,
    div_operator,
    rem_operator,
    eq_operator,
    noteq_operator,
    lt_operator,
    le_operator,
    gt_operator,
    ge_operator,
    and_operator,
    assign_operator,
    separator,
    or_operator,
    path_connector,
    arrow_operator,
    comma,
    statement_end,
    left_curly_brace,
    right_curly_brace,
    left_square_bracket,
    right_square_bracket,
    left_parenthesis,
    right_parenthesis,

    identifier,

    any,
}
}

#[derive(Clone, Copy)]
pub struct CharLocation {
    row: usize,
    col: usize
}

impl Default for CharLocation {
    fn default() -> Self {
        Self { row: 1, col: 0 }
    }
}

impl TokenLocation<char> for CharLocation {
    fn set_location(&mut self, c: &char) {
        if c == &'\n' {
            self.col = 0;
            self.row += 1;
        } else {
            self.col += 1;
        }
    }
    fn print_location(&self) {
        print!("({}, {})", self.row, self.col);
    }
}

pub fn print_tokens(tokens: &Vec<Token<char, LexicalRules, CharLocation>>, tab: String) {
    for token in tokens {
        match token {
            Token::Element(c, _) => {
                if WHITESPACE.contains(*c) {
                    if *c == '\n' {
                        print!("\\n");
                    } else {
                        print!("#");
                    }
                } else {
                    print!("{}", c);
                }
            },
            Token::Token(rule, sub_tokens, _) => {
                print!("\n{}{:?}: ", tab, rule);
                let mut sub_tab = tab.clone();
                sub_tab.push('\t');
                print_tokens(sub_tokens, sub_tab);
                // print!("\n");
            }
        }
    }
}


impl ConditionMatch<char> for &'static str {
    fn contains(&self, c: &char) -> bool {
        (*self).contains(*c)
    }
}

pub type Matcher = PatternMatcher<&'static str, char, LexicalRules, CharLocation>;
