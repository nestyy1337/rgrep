use std::cmp::Ordering;
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct NFA {
    pub states: Vec<State>,
    pub start_state: usize,
    pub accept_state: usize,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq)]
pub struct State {
    pub id: usize,
    pub transitions: Vec<Transitions>,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id.cmp(&other.id)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq)]
pub enum Transitions {
    Char { c: char, next: usize },
    NegatedCharSet { chars: Vec<char>, next: usize },
    Epsilon { next: usize },
    AnyChar { next: usize },
    StartAnchor { next: usize },
    EndAnchor { next: usize },
}

impl NFA {
    pub fn matches(&self, input: &str) -> bool {
        let input_chars: Vec<char> = input.chars().collect();
        for start in 0..=input_chars.len() {
            let mut current_states = self.epsilon_closure(HashSet::from([self.start_state]));

            let is_at_start = start == 0;
            current_states = self.process_anchors(current_states, is_at_start, false);

            let mut pos = start;
            let mut match_found = current_states.contains(&self.accept_state);

            while !match_found && pos < input_chars.len() {
                let c = input_chars[pos];
                let mut next_states = HashSet::new();

                for &state_id in &current_states {
                    if let Some(state) = self.states.iter().find(|s| s.id == state_id) {
                        for transition in &state.transitions {
                            match transition {
                                Transitions::Char { c: expected, next } => {
                                    if *expected == c {
                                        next_states.insert(*next);
                                    }
                                }
                                Transitions::NegatedCharSet { chars, next } => {
                                    if !chars.contains(&c) {
                                        next_states.insert(*next);
                                    }
                                }
                                Transitions::AnyChar { next } => {
                                    next_states.insert(*next);
                                }
                                _ => {}
                            }
                        }
                    }
                }

                pos += 1;
                let is_at_end = pos == input_chars.len();
                current_states = self.epsilon_closure(next_states);

                current_states = self.process_anchors(current_states, false, is_at_end);
                match_found = current_states.contains(&self.accept_state);
            }

            if match_found {
                return true;
            }
        }
        false
    }

    fn process_anchors(
        &self,
        states: HashSet<usize>,
        is_at_start: bool,
        is_at_end: bool,
    ) -> HashSet<usize> {
        let mut result = HashSet::new();

        for &state_id in &states {
            if let Some(state) = self.states.iter().find(|s| s.id == state_id) {
                let mut add_state = true;

                for transition in &state.transitions {
                    match transition {
                        Transitions::StartAnchor { next } => {
                            add_state = false;
                            if is_at_start {
                                result.insert(*next);
                            }
                        }
                        Transitions::EndAnchor { next } => {
                            add_state = false;
                            if is_at_end {
                                result.insert(*next);
                            }
                        }
                        _ => {}
                    }
                }

                if add_state {
                    result.insert(state_id);
                }
            }
        }

        self.epsilon_closure(result)
    }

    /// Returns the epsilon closure of the given set of state IDs
    pub fn epsilon_closure(&self, start_states: HashSet<usize>) -> HashSet<usize> {
        let mut states = start_states.clone();
        let mut stack: Vec<usize> = start_states.into_iter().collect();

        while let Some(state_id) = stack.pop() {
            if let Some(state) = self.states.iter().find(|s| s.id == state_id) {
                for transition in &state.transitions {
                    if let Transitions::Epsilon { next } = transition {
                        if states.insert(*next) {
                            stack.push(*next);
                        }
                    }
                }
            }
        }
        states
    }

    pub fn from_char(input: char, state_id: usize) -> Self {
        let start_id = state_id;
        let accept_id = state_id + 1;
        let states = vec![
            State {
                id: start_id,
                transitions: vec![Transitions::Char {
                    c: input,
                    next: accept_id,
                }],
            },
            State {
                id: accept_id,
                transitions: vec![],
            },
        ];
        NFA {
            states,
            start_state: start_id,
            accept_state: accept_id,
        }
    }
}
