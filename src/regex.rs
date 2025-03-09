use crate::nfa::{State, Transitions, NFA};

#[derive(Debug)]
pub struct RegexParser {
    input: Vec<char>,
    pos: usize,
}

impl RegexParser {
    pub fn new(input_str: &str) -> Self {
        RegexParser {
            input: input_str.chars().collect(),
            pos: 0,
        }
    }

    pub fn parse_regex(&mut self) -> Result<RegexAction, String> {
        self.parse_alternation()
    }

    fn parse_alternation(&mut self) -> Result<RegexAction, String> {
        let mut reg = self.parse_concat()?;
        while self.pos < self.input.len() && self.input[self.pos] == '|' {
            self.pos += 1;
            let right = self.parse_concat()?;
            reg = RegexAction::Alternation(Box::new(reg), Box::new(right));
        }
        Ok(reg)
    }

    fn parse_concat(&mut self) -> Result<RegexAction, String> {
        let mut reg = self.parse_repetition()?;
        while self.pos < self.input.len() && !matches!(self.input[self.pos], '|' | ')' | ']') {
            let next = self.parse_repetition()?;
            reg = RegexAction::Concatenation(Box::new(reg), Box::new(next));
        }
        Ok(reg)
    }

    fn parse_repetition(&mut self) -> Result<RegexAction, String> {
        let mut reg = self.parse_atom()?;
        if self.pos < self.input.len() {
            match self.input[self.pos] {
                '*' => {
                    self.pos += 1;
                    reg = RegexAction::Repetition(Box::new(reg));
                }
                '+' => {
                    self.pos += 1;
                    reg = RegexAction::OneOrMore(Box::new(reg));
                }
                '?' => {
                    self.pos += 1;
                    reg = RegexAction::Optional(Box::new(reg));
                }
                _ => {}
            }
        }
        Ok(reg)
    }

    fn parse_atom(&mut self) -> Result<RegexAction, String> {
        if self.pos >= self.input.len() {
            return Err("Unexpected end of regex pattern".to_string());
        }
        match self.input[self.pos] {
            '(' => {
                self.pos += 1;
                let reg = self.parse_regex()?;
                if self.pos >= self.input.len() || self.input[self.pos] != ')' {
                    return Err("Unclosed parenthesis".to_string());
                }
                self.pos += 1;
                Ok(reg)
            }
            '[' => {
                self.pos += 1;
                let is_negated = self.pos < self.input.len() && self.input[self.pos] == '^';
                if is_negated {
                    self.pos += 1;
                }
                let mut chars = Vec::new();
                while self.pos < self.input.len() && self.input[self.pos] != ']' {
                    chars.push(self.input[self.pos]);
                    self.pos += 1;
                }
                if self.pos >= self.input.len() {
                    return Err("Unclosed character class".to_string());
                }
                self.pos += 1; // skip the closing ']'
                Ok(RegexAction::CharClass(chars, is_negated))
            }
            '\\' => {
                // Handle escape sequences
                self.pos += 1;
                if self.pos >= self.input.len() {
                    return Err("Unexpected end of escape sequence".to_string());
                }

                match self.input[self.pos] {
                    'd' => {
                        self.pos += 1;
                        Ok(RegexAction::ShorthandClass(ShorthandType::Digit))
                    }
                    'D' => {
                        self.pos += 1;
                        Ok(RegexAction::ShorthandClass(ShorthandType::NotDigit))
                    }
                    'w' => {
                        self.pos += 1;
                        Ok(RegexAction::ShorthandClass(ShorthandType::Word))
                    }
                    'W' => {
                        self.pos += 1;
                        Ok(RegexAction::ShorthandClass(ShorthandType::NotWord))
                    }
                    's' => {
                        self.pos += 1;
                        Ok(RegexAction::ShorthandClass(ShorthandType::Whitespace))
                    }
                    'S' => {
                        self.pos += 1;
                        Ok(RegexAction::ShorthandClass(ShorthandType::NotWhitespace))
                    }
                    c => {
                        self.pos += 1;
                        Ok(RegexAction::Char(c))
                    }
                }
            }
            '^' => {
                self.pos += 1;
                Ok(RegexAction::StartAnchor)
            }
            '$' => {
                self.pos += 1;
                Ok(RegexAction::EndAnchor)
            }

            '.' => {
                self.pos += 1;
                Ok(RegexAction::AnyChar)
            }
            c => {
                self.pos += 1;
                Ok(RegexAction::Char(c))
            }
        }
    }
}

pub enum RegexAction {
    Char(char),
    Concatenation(Box<RegexAction>, Box<RegexAction>),
    Alternation(Box<RegexAction>, Box<RegexAction>),
    Repetition(Box<RegexAction>),
    OneOrMore(Box<RegexAction>),
    Optional(Box<RegexAction>),
    CharClass(Vec<char>, bool),
    ShorthandClass(ShorthandType),
    StartAnchor,
    EndAnchor,
    AnyChar,
}

#[derive(Debug)]
enum ShorthandType {
    Digit,         // \d - matches [0-9]
    Word,          // \w - matches [a-zA-Z0-9_]
    Whitespace,    // \s - matches [ \t\n\r\f]
    NotDigit,      // \D - matches [^0-9]
    NotWord,       // \W - matches [^a-zA-Z0-9_]
    NotWhitespace, // \S - matches [^ \t\n\r\f]
}

impl RegexAction {
    pub fn compile(regex: RegexAction, state_id: usize) -> NFA {
        match regex {
            RegexAction::Char(c) => NFA::from_char(c, state_id),
            RegexAction::Concatenation(left, right) => {
                let nfa_left = Self::compile(*left, state_id);
                let new_state_id = nfa_left.accept_state + 1;
                let nfa_right = Self::compile(*right, new_state_id);
                Self::concate(nfa_left, nfa_right)
            }
            RegexAction::Alternation(left, right) => {
                let mut left_nfa = Self::compile(*left, state_id + 1);
                let mut right_nfa = Self::compile(*right, left_nfa.accept_state + 1);
                let new_start = state_id;
                let new_accept = right_nfa.accept_state + 1;

                let start_state = State {
                    id: new_start,
                    transitions: vec![
                        Transitions::Epsilon {
                            next: left_nfa.start_state,
                        },
                        Transitions::Epsilon {
                            next: right_nfa.start_state,
                        },
                    ],
                };

                let accept_state = State {
                    id: new_accept,
                    transitions: vec![],
                };

                if let Some(state) = left_nfa
                    .states
                    .iter_mut()
                    .find(|s| s.id == left_nfa.accept_state)
                {
                    state
                        .transitions
                        .push(Transitions::Epsilon { next: new_accept });
                }
                if let Some(state) = right_nfa
                    .states
                    .iter_mut()
                    .find(|s| s.id == right_nfa.accept_state)
                {
                    state
                        .transitions
                        .push(Transitions::Epsilon { next: new_accept });
                }

                let mut combined_states = vec![start_state, accept_state];
                combined_states.extend(left_nfa.states);
                combined_states.extend(right_nfa.states);
                combined_states.sort();

                NFA {
                    states: combined_states,
                    start_state: new_start,
                    accept_state: new_accept,
                }
            }
            RegexAction::Repetition(reg) => {
                let mut inner = Self::compile(*reg, state_id);
                let start_id = state_id;
                let accept_id = inner.accept_state + 1;

                let start_state = State {
                    id: start_id,
                    transitions: vec![
                        Transitions::Epsilon { next: accept_id },
                        Transitions::Epsilon {
                            next: inner.start_state,
                        },
                    ],
                };

                let accept_state = State {
                    id: accept_id,
                    transitions: vec![],
                };

                if let Some(state) = inner.states.iter_mut().find(|s| s.id == inner.accept_state) {
                    state.transitions.push(Transitions::Epsilon {
                        next: inner.start_state,
                    });
                    state
                        .transitions
                        .push(Transitions::Epsilon { next: accept_id });
                }

                let mut combined_states = vec![start_state, accept_state];
                combined_states.extend(inner.states);
                combined_states.sort();

                NFA {
                    states: combined_states,
                    start_state: start_id,
                    accept_state: accept_id,
                }
            }
            RegexAction::OneOrMore(reg) => {
                let mut inner = Self::compile(*reg, state_id);
                let start_id = inner.start_state;
                let accept_id = inner.accept_state + 1;

                let accept_state = State {
                    id: accept_id,
                    transitions: vec![],
                };

                if let Some(state) = inner.states.iter_mut().find(|s| s.id == inner.accept_state) {
                    state.transitions.push(Transitions::Epsilon {
                        next: inner.start_state,
                    });
                    state
                        .transitions
                        .push(Transitions::Epsilon { next: accept_id });
                }

                let mut combined_states = inner.states;
                combined_states.push(accept_state);
                combined_states.sort();

                NFA {
                    states: combined_states,
                    start_state: start_id,
                    accept_state: accept_id,
                }
            }
            RegexAction::Optional(reg) => {
                let mut inner = Self::compile(*reg, state_id + 1);
                let start_id = state_id;
                let accept_id = inner.accept_state + 1;

                let start_state = State {
                    id: start_id,
                    transitions: vec![
                        Transitions::Epsilon { next: accept_id },
                        Transitions::Epsilon {
                            next: inner.start_state,
                        },
                    ],
                };

                let accept_state = State {
                    id: accept_id,
                    transitions: vec![],
                };

                if let Some(state) = inner.states.iter_mut().find(|s| s.id == inner.accept_state) {
                    state
                        .transitions
                        .push(Transitions::Epsilon { next: accept_id });
                }

                let mut combined_states = vec![start_state, accept_state];
                combined_states.extend(inner.states);
                combined_states.sort();

                NFA {
                    states: combined_states,
                    start_state: start_id,
                    accept_state: accept_id,
                }
            }
            RegexAction::CharClass(chars, is_negated) => {
                let start_id = state_id;
                let accept_id = state_id + 1;
                let transitions = if is_negated {
                    vec![Transitions::NegatedCharSet {
                        chars: chars.clone(),
                        next: accept_id,
                    }]
                } else {
                    chars
                        .into_iter()
                        .map(|c| Transitions::Char { c, next: accept_id })
                        .collect()
                };
                let start_state = State {
                    id: start_id,
                    transitions,
                };
                let accept_state = State {
                    id: accept_id,
                    transitions: vec![],
                };
                NFA {
                    states: vec![start_state, accept_state],
                    start_state: start_id,
                    accept_state: accept_id,
                }
            }
            RegexAction::StartAnchor => {
                let start_id = state_id;
                let accept_id = state_id + 1;

                let start_state = State {
                    id: start_id,
                    transitions: vec![Transitions::StartAnchor { next: accept_id }],
                };
                let accept_state = State {
                    id: accept_id,
                    transitions: vec![],
                };

                NFA {
                    states: vec![start_state, accept_state],
                    start_state: start_id,
                    accept_state: accept_id,
                }
            }

            RegexAction::EndAnchor => {
                let start_id = state_id;
                let accept_id = state_id + 1;

                let start_state = State {
                    id: start_id,
                    transitions: vec![Transitions::EndAnchor { next: accept_id }],
                };
                let accept_state = State {
                    id: accept_id,
                    transitions: vec![],
                };
                NFA {
                    states: vec![start_state, accept_state],
                    start_state: start_id,
                    accept_state: accept_id,
                }
            }
            RegexAction::ShorthandClass(shorthand_type) => {
                let start_id = state_id;
                let accept_id = state_id + 1;

                let transitions = match shorthand_type {
                    ShorthandType::Digit => {
                        // Digits 0-9
                        let digits = ('0'..='9').collect::<Vec<_>>();
                        digits
                            .into_iter()
                            .map(|c| Transitions::Char { c, next: accept_id })
                            .collect()
                    }
                    ShorthandType::Word => {
                        // word characters (letters, digits, underscore)
                        let word_chars = ('a'..='z')
                            .chain('A'..='Z')
                            .chain('0'..='9')
                            .chain(std::iter::once('_'))
                            .collect::<Vec<_>>();
                        word_chars
                            .into_iter()
                            .map(|c| Transitions::Char { c, next: accept_id })
                            .collect()
                    }
                    ShorthandType::Whitespace => {
                        // whitespace characters
                        let whitespace = vec![' ', '\t', '\n', '\r', '\u{000C}'];
                        whitespace
                            .into_iter()
                            .map(|c| Transitions::Char { c, next: accept_id })
                            .collect()
                    }
                    ShorthandType::NotDigit => {
                        vec![Transitions::NegatedCharSet {
                            chars: ('0'..='9').collect(),
                            next: accept_id,
                        }]
                    }
                    ShorthandType::NotWord => {
                        let word_chars = ('a'..='z')
                            .chain('A'..='Z')
                            .chain('0'..='9')
                            .chain(std::iter::once('_'))
                            .collect::<Vec<_>>();
                        vec![Transitions::NegatedCharSet {
                            chars: word_chars,
                            next: accept_id,
                        }]
                    }
                    ShorthandType::NotWhitespace => {
                        vec![Transitions::NegatedCharSet {
                            chars: vec![' ', '\t', '\n', '\r', '\u{000C}'],
                            next: accept_id,
                        }]
                    }
                };

                let start_state = State {
                    id: start_id,
                    transitions,
                };
                let accept_state = State {
                    id: accept_id,
                    transitions: vec![],
                };

                NFA {
                    states: vec![start_state, accept_state],
                    start_state: start_id,
                    accept_state: accept_id,
                }
            }
            RegexAction::AnyChar => {
                let start_id = state_id;
                let accept_id = state_id + 1;

                let start_state = State {
                    id: start_id,
                    transitions: vec![Transitions::AnyChar { next: accept_id }],
                };
                let accept_state = State {
                    id: accept_id,
                    transitions: vec![],
                };

                NFA {
                    states: vec![start_state, accept_state],
                    start_state: start_id,
                    accept_state: accept_id,
                }
            }
        }
    }

    fn concate(mut nfa1: NFA, nfa2: NFA) -> NFA {
        if let Some(state) = nfa1.states.iter_mut().find(|s| s.id == nfa1.accept_state) {
            state.transitions.push(Transitions::Epsilon {
                next: nfa2.start_state,
            });
        }
        let mut combined_states = nfa1.states;
        combined_states.extend(nfa2.states.into_iter());
        combined_states.sort();
        NFA {
            states: combined_states,
            start_state: nfa1.start_state,
            accept_state: nfa2.accept_state,
        }
    }
}

pub fn sanitize_regex_pattern(input: &str) -> String {
    let special_chars = [
        '.', '^', '$', '*', '+', '?', '(', ')', '[', ']', '{', '}', '|', '\\',
    ];
    let shorthand_classes = ['d', 'w', 's', 'D', 'W', 'S'];
    let mut result = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(&next) = chars.peek() {
                if special_chars.contains(&next) {
                    // Keep the special character without the backslash
                    chars.next();
                    result.push(next);
                } else if shorthand_classes.contains(&next) {
                    // Keep both the backslash and the shorthand class character
                    result.push(c);
                    result.push(chars.next().unwrap());
                } else {
                    // Just a backslash with no special meaning
                    result.push(c);
                }
            } else {
                // Trailing backslash
                result.push(c);
            }
        } else {
            result.push(c);
        }
    }
    result
}
