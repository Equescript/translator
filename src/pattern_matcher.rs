pub enum RuleInstruction<ElementToken, ResultToken, TokenConditions>
where
    &'static [(Option<(TokenConditions, bool)>, ResultToken, InstructionAddress)]: 'static,
{
    IsOnly(ElementToken),
    NotOnly(ElementToken),
    Is(TokenConditions), // Is("") 可被用于故意错误退出
    Not(TokenConditions), // Not("") 可被用于跳转后的占位匹配
    IsGoto(TokenConditions, InstructionAddress), // 匹配成功跳转，匹配不成功继续前进，除跳转到-1地址外，匹配过程不消耗字符，即会让本字符继续沿着下一条指令匹配，跳转到-1地址代表成功结束
    NotGoto(TokenConditions, InstructionAddress), // NotGoto("") 可用于构造loop
    Call(&'static [(Option<(TokenConditions, bool)>, ResultToken, InstructionAddress)], usize), // 调用其他匹配规则，推荐使用调用前提条件以提高效率
    ErrEnd(Option<ResultToken>), // 一个无限匹配规则遇见错误结尾，将正确部分返回，并让上一级matcher重新匹配最后在自己的规则下错误的字符
    /* DebugIs(TokenConditions, &'static str), // Is("") 可被用于故意错误退出
    DebugNot(TokenConditions, &'static str), // Not("") 可被用于跳转后的占位匹配
    DebugIsGoto(TokenConditions, InstructionAddress, &'static str), // 匹配成功跳转，匹配不成功继续前进，除跳转到-1地址外，匹配过程不消耗字符，即会让本字符继续沿着下一条指令匹配，跳转到-1地址代表成功结束
    DebugNotGoto(TokenConditions, InstructionAddress, &'static str), // NotGoto("") 可用于构造loop
    DebugCall(&'static [(ResultToken, InstructionAddress)], usize, &'static str),
    DebugErrEnd(&'static str), */
}

pub enum InstructionAddress {
    Absolute(i32),
    Relative(i32),
}

pub type IA = InstructionAddress;

pub const MATCH_TERMINAL: IA = IA::Absolute(-1);
pub const NEXT: IA = IA::Relative(1);

#[derive(Clone)]
pub enum Token<ElementToken, ResultToken, L> {
    Element(ElementToken, L),
    Token(ResultToken, Vec<Token<ElementToken, ResultToken, L>>, L)
}

pub trait TokenLocation<ElementToken>: Clone {
    fn set_location(&mut self, c: &ElementToken);
    fn print_location(&self) {}
}

macro_rules! ImplRulesInto {
    ($(#[$meta:meta])* $vis:vis enum $name:ident {
        $($(#[$vmeta:meta])* $vname:ident ),*
        $(,)?
    }) => {
        $(#[$meta])*
        $vis enum $name {
            $($(#[$vmeta])* $vname ),*
        }

        impl From<$name> for &[RI] {
            fn from(value: $name) -> Self {
                match value {
                    $($name::$vname => &$vname,)*
                }
            }
        }
    };
}

pub trait ConditionMatch<ElementToken> {
    fn contains(&self, c: &ElementToken) -> bool;
}

pub(crate) use ImplRulesInto;

#[derive(Clone)]
pub struct PatternMatcher<TokenConditions, ElementToken, ResultToken, L>
where
    TokenConditions: Clone + ConditionMatch<ElementToken>,
    ElementToken: Clone + Eq + 'static,
    ResultToken: Clone,
    L: Clone + TokenLocation<ElementToken>,
    &'static [RuleInstruction<ElementToken, ResultToken, TokenConditions>]: From<ResultToken> + 'static,
{
    pub token: Vec<Token<ElementToken, ResultToken, L>>,
    pub rule: ResultToken,
    rule_instructions: &'static [RuleInstruction<ElementToken, ResultToken, TokenConditions>],
    address: i32,
    pub location: L,
    pub sub_rule_matchers: Option<Vec<(Self, i32)>>,
    pub tab: String,
}

impl<TokenConditions, ElementToken, ResultToken, L> PatternMatcher<TokenConditions, ElementToken, ResultToken, L>
where
    TokenConditions: Clone + ConditionMatch<ElementToken>,
    ElementToken: Clone + Eq + 'static,
    ResultToken: Clone + std::fmt::Debug,
    L: Clone + TokenLocation<ElementToken>,
    &'static [RuleInstruction<ElementToken, ResultToken, TokenConditions>]: From<ResultToken>,
{
    pub fn new(rule: ResultToken, location: L, tab: String) -> Self {
        print!("{}", tab);
        location.print_location();
        print!("  [call] {:?}\n", rule);
        let rule_instructions = rule.clone().into();
        Self { token: Vec::new(), rule, rule_instructions, address: 0, location, sub_rule_matchers: None, tab }
    }
    fn match_instruction(&mut self, c: &ElementToken) -> Option<Result<Option<(Token<ElementToken, ResultToken, L>, bool)>, (Self, String)>> {
        macro_rules! DebugPrint {
            ($info: expr) => {
                /* print!("{}", self.tab);
                self.location.print_location();
                print!("  {}", $info); */
                println!("{}{}", self.tab, $info);
            };
        }
        loop {
            // match &self.rule_instructions.get(self.address as usize).unwrap()
            match match &self.rule_instructions.get(match <i32 as TryInto<usize>>::try_into(self.address) {
                Ok(t) => t,
                Err(_) => {
                    print!("{}", self.tab);
                    self.location.print_location();
                    print!("  {:?} error: cannot convert instruction address {} into usize!\n", self.rule, self.address);
                    panic!();
                    // return Some(Err(self.clone()))
                },
            }) {
                Some(t) => t,
                None => {
                    print!("{}", self.tab);
                    self.location.print_location();
                    print!("  {:?} error: get None at instruction address {}!\n", self.rule, self.address);
                    panic!();
                    // return Some(Err(self.clone()))
                },
            } {
                RuleInstruction::IsOnly(condition) => {
                    if condition == c {
                        self.token.push(Token::Element(c.clone(), self.location.clone()));
                        return Some(Ok(None))
                    } else {
                        return Some(Err((self.clone(), "Is instruction not match".to_string())))
                    }
                },
                RuleInstruction::NotOnly(condition) => {
                    if condition == c {
                        return Some(Err((self.clone(), "Not instruction not match".to_string())))
                    } else {
                        self.token.push(Token::Element(c.clone(), self.location.clone()));
                        return Some(Ok(None))
                    }
                },
                RuleInstruction::Is(conditions) => {
                    if conditions.contains(c) {
                        self.token.push(Token::Element(c.clone(), self.location.clone()));
                        return Some(Ok(None))
                    } else {
                        return Some(Err((self.clone(), "Is instruction not match".to_string())))
                    }
                },
                RuleInstruction::Not(conditions) => {
                    if conditions.contains(c) {
                        return Some(Err((self.clone(), "Not instruction not match".to_string())))
                    } else {
                        self.token.push(Token::Element(c.clone(), self.location.clone()));
                        return Some(Ok(None))
                    }
                },
                RuleInstruction::IsGoto(conditions, i) => {
                    if conditions.contains(c) {
                        let target_address = match i {
                            InstructionAddress::Absolute(i) => *i,
                            InstructionAddress::Relative(i) => self.address + i,
                        };
                        if target_address == -1 {
                            self.token.push(Token::Element(c.clone(), self.location.clone()));
                            return Some(Ok(Some((Token::Token(self.rule.clone(), self.token.clone(), self.location.clone()), false))));
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
                            self.token.push(Token::Element(c.clone(), self.location.clone()));
                            return Some(Ok(Some((Token::Token(self.rule.clone(), self.token.clone(), self.location.clone()), false))));
                        }
                        self.address = target_address;
                    }
                },
                RuleInstruction::Call(rules, length) => {
                    macro_rules! Calling {
                        ($sub_rule_matchers: expr) => {
                            for i in 0..*length {
                                match rules.get(i) {
                                    Some((precondition, rule, address)) => {
                                        if let Some((conditions, is_or_not)) = precondition {
                                            if conditions.contains(c) != *is_or_not {
                                                continue;
                                            }
                                        }
                                        let target_address = match address {
                                            InstructionAddress::Absolute(a) => *a,
                                            InstructionAddress::Relative(a) => self.address + a,
                                        };
                                        $sub_rule_matchers.push((Self::new(
                                            rule.clone(), self.location.clone(),
                                            {let mut new_tab = self.tab.clone(); new_tab.push('\t'); new_tab}
                                        ), target_address));
                                    },
                                    None => return Some(Err((self.clone(), "Call function get None".to_string())))
                                }
                            }
                        };
                    }
                    match &mut self.sub_rule_matchers {
                        Some(sub_rule_matchers) => {
                            Calling!(sub_rule_matchers);
                        },
                        None => {
                            let mut new_sub_rule_matchers = Vec::new();
                            Calling!(new_sub_rule_matchers);
                            self.sub_rule_matchers = Some(new_sub_rule_matchers);
                        }
                    }
                    return None; // 不设置地址不会影响结果，调用结束时会设置地址
                },
                RuleInstruction::ErrEnd(result_token) => {
                    return Some(Ok(Some((Token::Token(match result_token {
                        Some(r) => r.clone(),
                        None => self.rule.clone()
                    }, self.token.clone(), self.location.clone()), true))));
                },
                /* RuleInstruction::DebugIs(conditions, info) => {
                    DebugPrint!(info);
                    if conditions.contains(c) {
                        self.token.push(Token::Element(c.clone(), self.location.clone()));
                        return Some(Ok(None))
                    } else {
                        return Some(Err((self.clone(), "Is instruction not match".to_string())))
                    }
                },
                RuleInstruction::DebugNot(conditions, info) => {
                    DebugPrint!(info);
                    if conditions.contains(c) {
                        return Some(Err((self.clone(), "Not instruction not match".to_string())))
                    } else {
                        self.token.push(Token::Element(c.clone(), self.location.clone()));
                        return Some(Ok(None))
                    }
                },
                RuleInstruction::DebugIsGoto(conditions, i, info) => {
                    DebugPrint!(info);
                    if conditions.contains(c) {
                        let target_address = match i {
                            InstructionAddress::Absolute(i) => *i,
                            InstructionAddress::Relative(i) => self.address + i,
                        };
                        if target_address == -1 {
                            self.token.push(Token::Element(c.clone(), self.location.clone()));
                            return Some(Ok(Some((Token::Token(self.rule.clone(), self.token.clone(), self.location.clone()), false))));
                        }
                        self.address = target_address;
                    } else {
                        self.address += 1;
                    }
                },
                RuleInstruction::DebugNotGoto(conditions, i, info) => {
                    DebugPrint!(info);
                    if conditions.contains(c) {
                        self.address += 1;
                    } else {
                        let target_address = match i {
                            InstructionAddress::Absolute(i) => *i,
                            InstructionAddress::Relative(i) => self.address + i,
                        };
                        if target_address == -1 {
                            self.token.push(Token::Element(c.clone(), self.location.clone()));
                            return Some(Ok(Some((Token::Token(self.rule.clone(), self.token.clone(), self.location.clone()), false))));
                        }
                        self.address = target_address;
                    }
                },
                RuleInstruction::DebugCall(rules, length, info) => {
                    DebugPrint!(info);
                    match &mut self.sub_rule_matchers {
                        Some(sub_rule_matchers) => {
                            for i in 0..*length {
                                match rules.get(i) {
                                    Some((rule, address)) => {
                                        let target_address = match address {
                                            InstructionAddress::Absolute(a) => *a,
                                            InstructionAddress::Relative(a) => self.address + a,
                                        };
                                        sub_rule_matchers.push((Self::new(
                                            rule.clone(), self.location.clone(),
                                            {let mut new_tab = self.tab.clone(); new_tab.push('\t'); new_tab}
                                        ), target_address));
                                    },
                                    None => return Some(Err((self.clone(), "Call function get None".to_string())))
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
                                        new_sub_rule_matchers.push((Self::new(
                                            rule.clone(), self.location.clone(),
                                            {let mut new_tab = self.tab.clone(); new_tab.push('\t'); new_tab}
                                        ), target_address));
                                    },
                                    None => return Some(Err((self.clone(), "Call function get None".to_string())))
                                }
                            }
                            self.sub_rule_matchers = Some(new_sub_rule_matchers);
                        }
                    }
                    return None; // 不设置地址不会影响结果，调用结束时会设置地址
                },
                RuleInstruction::DebugErrEnd(info) => {
                    DebugPrint!(info);
                    return Some(Ok(Some((Token::Token(self.rule.clone(), self.token.clone(), self.location.clone()), true))));
                }, */
            }
        }
    }
    fn match_sub_rules(
        token: &mut Vec<Token<ElementToken, ResultToken, L>>,
        rule: & ResultToken,
        address: &mut i32,
        location: &mut L,
        sub_rule_matchers: &mut Vec<(Self, i32)>,
        c: &ElementToken,
        tab: &String,
    ) -> (Option<Vec<(Self, i32)>>, Option<Result<Option<(Token<ElementToken, ResultToken, L>, bool)>, (Self, String)>>) {
        // let length = sub_rule_matchers.len();
        let mut new_sub_rule_matchers = Vec::new();
        let mut result = None;
        let mut success = 0;
        // let mut success_rules = Vec::new();
        for (rule_matcher, target_address) in sub_rule_matchers {
            match rule_matcher.match_token(c) {
                Ok(option_token) => match option_token {
                    Some((t, is_err_end)) =>
                    {
                        result = Some((t, is_err_end, *target_address));
                        // success_rules.push(rule_matcher.rule.clone());
                        success += 1;
                    },
                    None => new_sub_rule_matchers.push((rule_matcher.clone(), *target_address)), // 收集未匹配完成的模式
                },
                Err((e, _)) => {
                    print!("{}", e.tab);
                    location.print_location();
                    print!("  [return] {:?} [unmatch]\n", e.rule);
                }, // 匹配错误的模式被直接丢弃
            }
        }
        /* if new_sub_rule_matchers.is_empty() {
            println!("success: {}", success);
            for rule in success_rules {
                println!("{:?}", rule)
            }
        } */
        if let Some((t, is_err_end, target_address)) = result {
            if success == 1 && new_sub_rule_matchers.is_empty() { // 只有当只剩最后一个sub_rule_matcher匹配成功，才能使用它的结果，否则丢弃（保证唯一解析）
                token.push(t);
                if target_address == -1 { // 最后唯一匹配成功，函数调用结束同时也是父级的结束，直接返回
                    return (None, Some(Ok(Some((Token::Token(rule.clone(), token.clone(), location.clone()), is_err_end)))));
                }
                *address = target_address; // 设置Call指令最后的跳转
                if is_err_end {
                    // sub_matcher是以err_end指令结束的，需要父级来重新匹配最后一个字符
                    return (None, None);
                } else {
                    // 子模式调用匹配完毕，重置状态
                    return (None, Some(Ok(None)));
                }
            }
        }
        (Some(new_sub_rule_matchers), Some(Ok(None))) // 还有多个匹配正在进行中，未决定匹配结果，需要继续匹配
    }
    pub fn match_token(&mut self, c: &ElementToken) -> Result<Option<(Token<ElementToken, ResultToken, L>, bool)>, (Self, String)> {
        self.location.set_location(c);
        loop {
            match &mut self.sub_rule_matchers {
                Some(sub_rule_matchers) => {
                    /* if sub_rule_matchers.len() > 1 {
                        println!("{:?} sub_rule_matchers: {}", self.rule, sub_rule_matchers.len());
                    } */
                    let (new_sub_rule_matchers, result) =
                    // 进行了函数调用的一次匹配
                    Self::match_sub_rules(&mut self.token, &self.rule, &mut self.address, &mut self.location, sub_rule_matchers, c, &self.tab);

                    if let Some(m) = &new_sub_rule_matchers {
                        if m.is_empty() {
                            return Err((self.clone(), "The function call have no match!".to_string())); // 没有任何模式被匹配上，返回错误
                        }
                    }
                    self.sub_rule_matchers = new_sub_rule_matchers;
                    if let Some(r) = result {
                        return r; // 有结果，函数调用返回，匹配结束
                    }
                }
                None => {
                    let result = self.match_instruction(c);
                    self.address += 1;
                    if let Some(r) = result {
                        return r;
                    }
                },
            }
        }
    }
}

