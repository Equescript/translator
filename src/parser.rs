use std::str::Chars;


/* '\u{E000}'
U+0009 (horizontal tab, '\t')
U+000A (line feed, '\n')
U+000B (vertical tab)
U+000C (form feed)
U+000D (carriage return, '\r')
U+0020 (space, ' ')
U+0085 (next line)
U+200E (left-to-right mark)
U+200F (right-to-left mark)
U+2028 (line separator)
U+2029 (paragraph separator) */
fn is_whitespace(c: char) -> bool {
    match c {
        '\u{0009}' => true,
        '\u{000A}' => true,
        '\u{000B}' => true,
        '\u{000C}' => true,
        '\u{000D}' => true,
        '\u{0020}' => true,
        '\u{0085}' => true,
        '\u{200E}' => true,
        '\u{200F}' => true,
        '\u{2028}' => true,
        '\u{2029}' => true,
        _ => false
    }
}

pub enum StructerType {
    Literal,
    SingleIdentifier,
    Identifier,
    ExpressionOnly,
    Application,
    Expression,
    Assignment,
    Statement,
}

struct PossibleType {
    literal: bool,
    single_identifier: bool,
    identifier: bool,
    expression_only: bool,
    application: bool,
    expression: bool,
    assignment: bool,
    statement: bool,
}

