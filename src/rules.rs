pub const NUMBER: &str = "0123456789";
pub const HEX_NUMBER: &str = "0123456789ABCDEFabcdef";
pub const WHITESPACE: &str = "\u{0009}\u{000A}\u{000B}\u{000C}\u{000D}\u{0020}\u{0085}\u{200E}\u{200F}\u{2028}\u{2029}";
pub const INVALID_CHAR: &str = "\\\"$#@~`^./";
pub const INVALID_IDENTIFIER_CHAR: &str = "\\\"$#@~`^./,;:!*/%+-<>=&|{}[]()";
pub const MATCH_TERMINAL: InstructionAddress = IA::Absolute(-1);
pub const NEXT: InstructionAddress = IA::Relative(1);

const expression_or_complex_expression: RI = RI::Call(&[(Rules::expression, NEXT), (Rules::complex_expression, NEXT)], 2);

/* macro_rules! RuleMatchProgram {
    ($( $name: ident: [ $($instruction: expr),* $(,)? ] $(;)? )*) => {
        fn rules() {
        $(let $name=();)*
        $(
        let $name/* : [RuleInstruction; 0usize $(+ {$instruction; 1usize} )*]  */= [
            $($instruction),*
        ];
        )*
        }
    };
} */

macro_rules! DefaultCall {
    (($call: ident, $address: expr)) => {
        (crate::rules::Rules::$call, $address)
    };
    ($call: ident) => {
        (crate::rules::Rules::$call, NEXT)
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
            (crate::rules::Rules::$call, $address)
        ], 1usize)
    };
    ($call_instruction: ident) => {
        RI::Call(&[
            (crate::rules::Rules::$call_instruction, NEXT)
        ], 1usize)
    };
}

pub(crate) use RICall;

fn rules() {

}



const w: [RuleInstruction; 21] = [
    RI::NotGoto(WHITESPACE, IA::Relative(3)),
    RI::Not(""),
    RI::NotGoto("", IA::Relative(-2)),
    RI::IsGoto("/", IA::Relative(2)),
    RI::ErrEnd, // 不是空格也不是 / 就结束
    RI::Not(""), // 占位匹配 /
    RI::IsGoto("/", IA::Relative(9)),
    RI::Is("*"),
    RI::IsGoto("*", IA::Relative(3)),
    RI::Not(""),
    RI::NotGoto("", IA::Relative(-2)),
    RI::Not(""), // 占位匹配 *
    RI::NotGoto("/", IA::Relative(-4)), // 下一个不是 / 就返回正常的匹配
    RI::Not(""), // 占位匹配 /
    RI::NotGoto("", IA::Absolute(0)), // 块注释结束，返回空格匹配
    RI::Not(""), // 占位匹配 /
    RI::IsGoto("\n", IA::Relative(3)), // 换行代表行注释结束
    RI::Not(""), // 匹配任何字符
    RI::NotGoto("", IA::Relative(-2)),
    RI::Not(""), // 占位匹配 \n
    RI::NotGoto("", IA::Absolute(0)), // 行注释结束，返回空格匹配
];

const number_str: [RuleInstruction; 5] = [
    RI::Is(NUMBER),
    RI::NotGoto(NUMBER, IA::Relative(4)),
    RI::Not(""),
    RI::NotGoto("", IA::Relative(-2)),
    RI::ErrEnd,
];

const hex_str: [RuleInstruction; 5] = [
    RI::Is(HEX_NUMBER),
    RI::NotGoto(HEX_NUMBER, IA::Relative(4)),
    RI::Not(""),
    RI::NotGoto("", IA::Relative(-2)),
    RI::ErrEnd,
];

const bool_literal: [RuleInstruction; 11] = [
    RI::IsGoto("f", InstructionAddress::Relative(5)),
    RI::Is("t"),
    RI::Is("r"),
    RI::Is("u"),
    RI::IsGoto("e", MATCH_TERMINAL),
    RI::Is("f"),
    RI::Is("a"),
    RI::Is("l"),
    RI::Is("s"),
    RI::IsGoto("e", MATCH_TERMINAL),
    RI::Is(""),
];

const char_literal: [RuleInstruction; 14] = [
    RI::Is("'"),
    RI::IsGoto("'\\", IA::Relative(3)),
    RI::Not("'\\"),
    RI::IsGoto("'", MATCH_TERMINAL),
    RI::Is("\\"),
    RI::IsGoto("u", IA::Relative(3)),
    RI::Is("0nrt'\\"),
    RI::IsGoto("'", MATCH_TERMINAL),
    RI::Is("u"),
    RI::Is("{"),
    RICall!(hex_str),
    RI::Is("}"),
    RI::IsGoto("'", MATCH_TERMINAL),
    RI::Is(""),
];

const i32_literal: [RuleInstruction; 5] = [
    RICall!(number_str),
    RI::Is("i"),
    RI::Is("3"),
    RI::IsGoto("2", MATCH_TERMINAL),
    RI::Is(""),
];

const usize_literal: [RuleInstruction; 7] = [
    RICall!(number_str),
    RI::Is("u"),
    RI::Is("s"),
    RI::Is("i"),
    RI::Is("z"),
    RI::IsGoto("e", MATCH_TERMINAL),
    RI::Is(""),
];

const f64_literal: [RuleInstruction; 8] = [
    RICall!(number_str),
    RI::IsGoto("f", IA::Relative(3)),
    RI::Is("."),
    RICall!(number_str),
    RI::Is("f"),
    RI::Is("6"),
    RI::IsGoto("4", MATCH_TERMINAL),
    RI::Is(""),
];

const literal: [RuleInstruction; 1] = [
    RICall!([
        (bool_literal, MATCH_TERMINAL),
        (char_literal, MATCH_TERMINAL),
        (i32_literal, MATCH_TERMINAL),
        (usize_literal, MATCH_TERMINAL),
        (f64_literal, MATCH_TERMINAL),
    ])
];

const identifier_str: [RuleInstruction; 6] = [
    RI::IsGoto(NUMBER, IA::Relative(5)),
    RI::IsGoto(INVALID_IDENTIFIER_CHAR, IA::Relative(4)),
    RI::Not(""),
    RI::NotGoto(INVALID_IDENTIFIER_CHAR, IA::Relative(-1)),
    RI::ErrEnd,
    RI::Is(""),
];

const simple_identifier: [RuleInstruction; 2] = [
    RICall!(identifier_str),
    RI::ErrEnd,
];

const identifier: [RuleInstruction; 7] = [
    RICall!(identifier_str),
    RI::NotGoto(":", IA::Relative(7)),
    RI::Is(""),
    RI::Is(":"),
    RICall!(identifier_str),
    RI::IsGoto(":", IA::Relative(-3)),
    RI::ErrEnd,
];

const obj: [RuleInstruction; 7] = [
    RI::Is("o"),
    RI::Is("b"),
    RI::Is("j"),
    RI::Is(WHITESPACE),
    RICall!(w),
    RICall!(simple_identifier),
    RI::ErrEnd,
];

const pair: [RuleInstruction; 13] = [
    RI::Is("("),
    RICall!(w),
    RICall!(expression),
    RICall!(w),
    RI::Is(","),
    RICall!(w),
    RICall!(expression),
    RICall!(w),
    RI::IsGoto(")", MATCH_TERMINAL),
    RI::Is(","),
    RICall!(w),
    RI::IsGoto(")", MATCH_TERMINAL),
    RI::Is("")
];

const tuple: [RuleInstruction; 12] = [
    RI::Is("["),
    RICall!(w),
    RICall!(expression),
    RICall!(w),
    RI::IsGoto("]", MATCH_TERMINAL),
    RI::Is(","),
    RICall!(w),
    RI::IsGoto("]", MATCH_TERMINAL),
    RICall!(expression),
    RICall!(w),
    RI::NotGoto("", IA::Relative(-6)),
    RI::Is(""),
];

const enum_set: [RuleInstruction; 16] = [
    RI::Is("{"),
    RICall!(w),
    RI::IsGoto("}", MATCH_TERMINAL),
    RICall!(expression),
    RICall!(w),
    RI::NotGoto(",", IA::Relative(6)),
    RI::Not(""),
    RICall!(w),
    RICall!(expression),
    RICall!(w),
    RI::NotGoto("", IA::Relative(-5)),
    RI::IsGoto("}", MATCH_TERMINAL),
    RI::Is(","),
    RICall!(w),
    RI::IsGoto("}", MATCH_TERMINAL),
    RI::Is("")
];

const conditional_set: [RuleInstruction; 11] = [
    RI::Is("{"),
    RICall!(w),
    RICall!(expression),
    RICall!(w),
    RI::Is("|"),
    RICall!(w),
    RI::IsGoto("}", MATCH_TERMINAL),
    expression_or_complex_expression,
    RICall!(w),
    RI::IsGoto("}", MATCH_TERMINAL),
    RI::Is("")
];

const map: [RuleInstruction; 16] = [
    RI::Is("m"),
    RI::Is("a"),
    RI::Is("p"),
    RICall!(w),
    RI::Is("{"),
    RICall!(w),
    RICall!(pair),
    RICall!(w),
    RI::IsGoto("}", MATCH_TERMINAL),
    RI::Is(","),
    RICall!(w),
    RI::IsGoto("}", MATCH_TERMINAL),
    RICall!(pair),
    RICall!(w),
    RI::NotGoto("", IA::Relative(-6)),
    RI::Is(""),
];

const func: [RuleInstruction; 23] = [
    RI::Is("f"),
    RI::Is("u"),
    RI::Is("n"),
    RI::Is("c"),
    RICall!(w),
    RI::Is("{"),
    RICall!(w),
    RICall!(expression),
    RICall!(w),
    RI::Is("-"),
    RI::Is(">"),
    RICall!(w),
    RI::IsGoto("_", IA::Relative(3)),
    expression_or_complex_expression,
    RI::NotGoto("", IA::Relative(2)),
    RI::Is(""), // _
    RICall!(w),
    RI::Is("|"),
    RICall!(w),
    RICall!(expression),
    RICall!(w),
    RI::IsGoto("}", MATCH_TERMINAL),
    RI::Is(""),
];

const container: [RuleInstruction; 6] = [
    RI::Is("("),
    RICall!(w),
    expression_or_complex_expression,
    RICall!(w),
    RI::IsGoto(")", MATCH_TERMINAL),
    RI::Is(""),
];

const expression_block_end: [RuleInstruction; 4] = [
    expression_or_complex_expression,
    RICall!(w),
    RI::IsGoto("}", MATCH_TERMINAL),
    RI::Is(""),
];

const expression_block: [RuleInstruction; 5] = [
    RI::Is("{"),
    RICall!(w),
    RI::Call(&[(Rules::statement, NEXT), (Rules::expression_block_end, MATCH_TERMINAL)], 2),
    RICall!(w),
    RI::NotGoto("", IA::Relative(-2)),
];

const expression: [RuleInstruction; 1] = [
    RICall!([
        (literal, MATCH_TERMINAL),
        (identifier, MATCH_TERMINAL),
        (obj, MATCH_TERMINAL),
        (pair, MATCH_TERMINAL),
        (tuple, MATCH_TERMINAL),
        (enum_set, MATCH_TERMINAL),
        (conditional_set, MATCH_TERMINAL),
        (map, MATCH_TERMINAL),
        (func, MATCH_TERMINAL),
        (container, MATCH_TERMINAL),
        (expression_block, MATCH_TERMINAL),
    ])
];

const application: [RuleInstruction; 7] = [
    RI::Is("["),
    RICall!(w),
    RICall!(expression),
    RICall!(w),
    RI::Is("]"),
    RICall!(w),
    RICall!(expression, MATCH_TERMINAL),
];

const index_expression: [RuleInstruction; 8] = [
    RICall!(expression, MATCH_TERMINAL),
    RICall!(w),
    RI::Is("["),
    RICall!(w),
    RICall!(expression),
    RICall!(w),
    RI::IsGoto("]", MATCH_TERMINAL),
    RI::Is(""),
];

const unary_operation: [RuleInstruction; 7] = [
    RI::IsGoto("-", IA::Relative(4)),
    RI::Is("!"),
    RICall!(w),
    RICall!(expression, MATCH_TERMINAL),
    RI::Not(""),
    RICall!(w),
    RICall!(expression, MATCH_TERMINAL),
];

macro_rules! BinaryOperation {
    ([$( ($name: ident, $operator: expr $(, $operator2: expr)? $(,)?) ),* $(,)*]) => {
        $(
        const $name: [RuleInstruction; 3usize $(+ {$operator2; 1usize})?] = [
            RI::Is($operator),
            $(RI::Is($operator2),)?
            RICall!(w),
            RICall!(expression, MATCH_TERMINAL),
        ];
        )*

        const operation_end: [RuleInstruction; 1] = [
            RICall!([
                $(($name, MATCH_TERMINAL)),*
            ])
        ];
    };
}

BinaryOperation!{[
    (_add, "+"),
    (_sub, "-"),
    (_mul, "*"),
    (_div, "/"),
    (_rem, "%"),
    (_eq, "=", "="),
    (_noteq, "!", "="),
    (_lt, "<"),
    (_gt, ">"),
    (_le, "<", "="),
    (_ge, ">", "="),
    (_and, "&", "&"),
    (_or, "|", "|"),
    (_in, "i", "n"),
    (_as, "a", "s"),
]}

const binary_operation: [RuleInstruction; 3] = [
    RICall!(expression),
    RICall!(w),
    RICall!(operation_end, MATCH_TERMINAL),
];

const complex_expression: [RuleInstruction; 1] = [
    RICall!([
        (application, MATCH_TERMINAL),
        (index_expression, MATCH_TERMINAL),
        (unary_operation, MATCH_TERMINAL),
        (binary_operation, MATCH_TERMINAL),
    ]),
];

const assignment: [RuleInstruction; 13] = [
    RI::Is("d"),
    RI::Is("e"),
    RI::Is("f"),
    RI::Is(WHITESPACE),
    RICall!(w),
    RICall!(simple_identifier),
    RICall!(w),
    RI::Is("="),
    RICall!(w),
    RICall!([
        expression,
        complex_expression
    ]),
    RICall!(w),
    RI::IsGoto(";", MATCH_TERMINAL),
    RI::Is(""),
];

const module_end: [RuleInstruction; 4] = [
    RICall!(statement),
    RICall!(w),
    RI::IsGoto("}", MATCH_TERMINAL),
    RI::Is(""),
];

const module: [RuleInstruction; 12] = [
    RI::Is("m"),
    RI::Is("o"),
    RI::Is("d"),
    RI::Is(WHITESPACE),
    RICall!(w),
    RICall!(simple_identifier),
    RICall!(w),
    RI::Is("{"),
    RICall!(w),
    RI::Call(&[(Rules::statement, NEXT), (Rules::module_end, MATCH_TERMINAL)], 2),
    RICall!(w),
    RI::NotGoto("", IA::Relative(-2)),
];

const empty_module: [RuleInstruction; 11] = [
    RI::Is("m"),
    RI::Is("o"),
    RI::Is("d"),
    RI::Is(WHITESPACE),
    RICall!(w),
    RICall!(simple_identifier),
    RICall!(w),
    RI::Is("{"),
    RICall!(w),
    RI::IsGoto("}", MATCH_TERMINAL),
    RI::Is(""),
];

const expression_statement: [RuleInstruction; 4] = [
    expression_or_complex_expression,
    RICall!(w),
    RI::IsGoto(";", MATCH_TERMINAL),
    RI::Is(""),
];

const statement: [RuleInstruction; 1] = [
    RICall!([
        (expression_statement, MATCH_TERMINAL),
        (assignment, MATCH_TERMINAL),
        (module, MATCH_TERMINAL),
        (empty_module, MATCH_TERMINAL),
    ]),
];



macro_rules! ImplRulesInto {
    ($(#[$meta:meta])* $vis:vis enum $name:ident {
        $($(#[$vmeta:meta])* $vname:ident ),*
        $(,)?
    }) => {
        $(#[$meta])*
        $vis enum $name {
            $($(#[$vmeta])* $vname ),*
        }

        impl From<&$name> for &[RuleInstruction] {
            fn from(value: &$name) -> Self {
                match value {
                    $($name::$vname => &$vname,)*
                    // _ => todo!()
                }
            }
        }
    };
}

ImplRulesInto!{
#[derive(Clone, Copy, Debug)]
pub enum Rules {
    w,
    number_str,
    hex_str,

    bool_literal,
    char_literal,
    i32_literal,
    usize_literal,
    f64_literal,

    literal,

    identifier_str,
    simple_identifier,
    identifier,

    obj,
    pair,
    tuple,
    enum_set,
    conditional_set,
    map,
    func,
    container,
    expression_block_end,
    expression_block,

    expression,

    application,
    index_expression,
    unary_operation,
    _add,
    _sub,
    _mul,
    _div,
    _rem,
    _eq,
    _noteq,
    _lt,
    _gt,
    _le,
    _ge,
    _and,
    _or,
    _in,
    _as,
    operation_end,
    binary_operation,

    complex_expression,

    assignment,
    module_end,
    module,
    empty_module,
    expression_statement,
    statement,
}
}


pub enum RuleInstruction {
    Is(&'static str), // Is("") 可被用于故意错误退出
    Not(&'static str), // Not("") 可被用于跳转后的占位匹配
    IsGoto(&'static str, InstructionAddress), // 匹配成功跳转，匹配不成功继续前进，除跳转到-1地址外，匹配过程不消耗字符，即会让本字符继续沿着下一条指令匹配，跳转到-1地址代表成功结束
    NotGoto(&'static str, InstructionAddress), // NotGoto("") 可用于构造loop
    Call(&'static [(Rules, InstructionAddress)], usize),
    ErrEnd, // 一个无限匹配规则遇见错误结尾，将正确部分返回，并让上一级matcher重新匹配最后在自己的规则下错误的字符
}

pub type RI = RuleInstruction;

pub enum InstructionAddress {
    Absolute(i32),
    Relative(i32),
}

pub type IA = InstructionAddress;

#[derive(Clone)]
enum Token {
    Char(char),
    Token(Rules, Vec<Token>)
}

fn print_token(token: &Token, tab: String) {
    match token {
        Token::Char(c) => {
            print!(" {}", c)
        },
        Token::Token(rule, tokens) => {
            print!("\n{}{:?}: ", tab, rule);
            let mut sub_tab = tab.clone();
            sub_tab.push('\t');
            for token in tokens {
                print_token(token, sub_tab.clone());
            }
            print!("\n{}", tab);
        },
    }

}

#[derive(Clone)]
struct RuleMatcher {
    token: Vec<Token>,
    rule: Rules,
    rule_instructions: &'static [RuleInstruction],
    address: i32,
    row: usize,
    column: usize,
    sub_rule_matchers: Option<Vec<(RuleMatcher, i32)>>,
}


impl RuleMatcher {
    fn new(rule: Rules, row: usize, column: usize) -> Self {
        Self { token: Vec::new(), rule, rule_instructions: (&rule).into(), address: 0, row, column, sub_rule_matchers: None }
    }
    fn move_location(row: &mut usize, column: &mut usize, c: char) {
        if c == '\n' {
            *row = 0;
            *column += 1;
        } else {
            *row += 1;
        }
    }
    fn match_instruction(&mut self, c: char) -> Option<Result<Option<(Token, bool)>, RuleMatcher>> {
        loop {
            match &self.rule_instructions.get(self.address as usize).unwrap()
            /* match &self.rule_instructions.get(match <i32 as TryInto<usize>>::try_into(self.address) {
                Ok(t) => t,
                Err(_) => return Some(Err(())),
            }) {
                Some(t) => t,
                None => return Some(Err(())),
            } */ {
                RuleInstruction::Is(conditions) => {
                    if conditions.contains(c) {
                        Self::move_location(&mut self.row, &mut self.column, c);
                        self.token.push(Token::Char(c));
                        return Some(Ok(None))
                    } else {
                        return Some(Err(self.clone()))
                    }
                },
                RuleInstruction::Not(conditions) => {
                    if conditions.contains(c) {
                        return Some(Err(self.clone()))
                    } else {
                        Self::move_location(&mut self.row, &mut self.column, c);
                        self.token.push(Token::Char(c));
                        return Some(Ok(None))
                    }
                }
                RuleInstruction::IsGoto(conditions, i) => {
                    if conditions.contains(c) {
                        let target_address = match i {
                            InstructionAddress::Absolute(i) => *i,
                            InstructionAddress::Relative(i) => self.address + i,
                        };
                        if target_address == -1 {
                            Self::move_location(&mut self.row, &mut self.column, c);
                            self.token.push(Token::Char(c));
                            return Some(Ok(Some((Token::Token(self.rule, self.token.clone()), false))));
                        }
                        self.address = target_address;
                    } else {
                        self.address += 1;
                    }
                },
                RuleInstruction::NotGoto(conditions, i) => {
                    if conditions.contains(c) {
                        self.address += 1;
                    } else {
                        let target_address = match i {
                            InstructionAddress::Absolute(i) => *i,
                            InstructionAddress::Relative(i) => self.address + i,
                        };
                        if target_address == -1 {
                            Self::move_location(&mut self.row, &mut self.column, c);
                            self.token.push(Token::Char(c));
                            return Some(Ok(Some((Token::Token(self.rule, self.token.clone()), false))));
                        }
                        self.address = target_address;
                    }
                },
                RuleInstruction::Call(rules, length) => {
                    match &mut self.sub_rule_matchers {
                        Some(sub_rule_matchers) => {
                            for i in 0..*length {
                                match rules.get(i) {
                                    Some((rule, address)) => {
                                        let target_address = match address {
                                            InstructionAddress::Absolute(a) => *a,
                                            InstructionAddress::Relative(a) => self.address + a,
                                        };
                                        sub_rule_matchers.push((Self::new(*rule, self.row, self.column), target_address));
                                    },
                                    None => return Some(Err(self.clone()))
                                }
                            }
                        },
                        None => {
                            let mut new_sub_rule_matchers = Vec::new();
                            for i in 0..*length {
                                match rules.get(i) {
                                    Some((rule, address)) => {
                                        let target_address = match address {
                                            InstructionAddress::Absolute(a) => *a,
                                            InstructionAddress::Relative(a) => self.address + a,
                                        };
                                        new_sub_rule_matchers.push((Self::new(*rule, self.row, self.column), target_address));
                                    },
                                    None => return Some(Err(self.clone()))
                                }
                            }
                            self.sub_rule_matchers = Some(new_sub_rule_matchers);
                        }
                    }
                    return None;
                },
                RuleInstruction::ErrEnd => {
                    return Some(Ok(Some((Token::Token(self.rule, self.token.clone()), true))));
                }
            }
        }
    }
    fn match_sub_rules(
        token: &mut Vec<Token>,
        rule: Rules,
        address: &mut i32,
        row: &mut usize,
        column: &mut usize,
        sub_rule_matchers: &mut Vec<(RuleMatcher, i32)>,
        c: char
    ) -> (Option<Vec<(RuleMatcher, i32)>>, Option<Result<Option<(Token, bool)>, RuleMatcher>>) {
        let length = sub_rule_matchers.len();
        let mut new_sub_rule_matchers = Vec::new();
        for (rule_matcher, target_address) in sub_rule_matchers {
            match rule_matcher.match_char(c) {
                Ok(option_token) => match option_token {
                    Some((t, is_err_end)) => if length == 1 { // 只有当只剩最后一个sub_rule_matcher才能使用它的匹配成功结果，否则丢弃（保证唯一解析）
                        Self::move_location(row, column, c);
                        token.push(t);
                        if *target_address == -1 { // 最后唯一匹配成功，直接结束
                            return (None, Some(Ok(Some((Token::Token(rule, token.clone()), is_err_end)))));
                        }
                        *address = *target_address; // 执行Call指令最后的跳转
                        if is_err_end { // sub_matcher是以err_end指令结束的，需要父级来重新匹配最后一个字符
                            return (None, None);
                        }
                        // 返回值中 Option<Vec<(RuleMatcher, i32)>> = None 子模式调用匹配完毕，重置状态
                        return (None, Some(Ok(None)));

                    },
                    None => new_sub_rule_matchers.push((rule_matcher.clone(), *target_address)), // 收集未匹配完成的模式
                },
                Err(_) => {}, // 匹配错误的模式被直接丢弃
            }
        }
        (Some(new_sub_rule_matchers), Some(Ok(None))) // 还有多个匹配正在进行中，未决定匹配结果，需要继续匹配
    }
    fn match_char(&mut self, c: char) -> Result<Option<(Token, bool)>, RuleMatcher> {
        loop {
            match &mut self.sub_rule_matchers {
                Some(sub_rule_matchers) => {
                    let (new_sub_rule_matchers, result) =
                    Self::match_sub_rules(&mut self.token, self.rule, &mut self.address, &mut self.row, &mut self.column, sub_rule_matchers, c);
                    if let Some(m) = &new_sub_rule_matchers {
                        if m.is_empty() {
                            return Err(self.clone()); // 没有任何模式被匹配上，返回错误
                        }
                    }
                    self.sub_rule_matchers = new_sub_rule_matchers;
                    if let Some(r) = result {
                        return r; // 匹配结束或继续匹配下一个字符
                    }
                }
                None => {
                    if let Some(r) = self.match_instruction(c) {
                        return r;
                    }
                    self.address += 1;
                },
            }
        }
    }
}

