// Copyright 2012-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Code for creating styled buffers

use crate::snippet::{Style, StyledString};

#[derive(Default)]
pub struct StyledBuffer {
    text: Vec<Vec<char>>,
    styles: Vec<Vec<Style>>,
}

impl StyledBuffer {
    pub fn copy_tabs(&mut self, row: usize) {
        if row < self.text.len() {
            for i in row + 1..self.text.len() {
                for j in 0..self.text[i].len() {
                    if self.text[row].len() > j
                        && self.text[row][j] == '\t'
                        && self.text[i][j] == ' '
                    {
                        self.text[i][j] = '\t';
                    }
                }
            }
        }
    }

    pub fn render(mut self) -> Vec<StyledString> {
        let mut output: Vec<StyledString> = vec![];

        // before we render, do a little patch-up work to support tabs
        self.copy_tabs(3);

        for (row, row_style) in self.text.iter().zip(&self.styles) {
            let mut current_style = Style::None;
            let mut current_text = String::new();

            for (&c, &s) in row.iter().zip(row_style) {
                if s != current_style {
                    if !current_text.is_empty() {
                        output.push(StyledString {
                            text: current_text,
                            style: current_style,
                        });
                    }
                    current_style = s;
                    current_text = String::new();
                }
                current_text.push(c);
            }
            if !current_text.is_empty() {
                output.push(StyledString {
                    text: current_text,
                    style: current_style,
                });
            }

            // We're done with the row, add a newline and keep going
            output.push(StyledString {
                text: "\n".to_owned(),
                style: Style::None,
            });
        }

        output
    }

    fn ensure_lines(&mut self, line: usize) {
        if self.text.len() <= line {
            self.text.resize(line + 1, Vec::new());
            self.styles.resize(line + 1, Vec::new());
        }
    }

    pub fn puts(&mut self, line: usize, col: usize, string: &str, style: Style) {
        self.ensure_lines(line);

        let text = &mut self.text[line];
        let styles = &mut self.styles[line];

        if text.len() < col {
            text.resize(col, ' ');
            styles.resize(col, Style::None);
        }

        let count = string.chars().count();
        let end = text.len().min(col + count);
        text.splice(col..end, string.chars());
        styles.splice(col..end, std::iter::repeat_n(style, count));
    }

    pub fn prepend(&mut self, line: usize, string: &str, style: Style) {
        self.ensure_lines(line);

        self.styles[line].splice(0..0, std::iter::repeat_n(style, string.chars().count()));
        self.text[line].splice(0..0, string.chars());
    }

    pub fn append(&mut self, line: usize, string: &str, style: Style) {
        self.ensure_lines(line);

        let text = &mut self.text[line];
        text.extend(string.chars());
        self.styles[line].resize(text.len(), style);
    }

    pub fn num_lines(&self) -> usize {
        self.text.len()
    }
}
