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

#[derive(Debug, Default)]
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

    pub fn render(mut self) -> Vec<Vec<StyledString>> {
        let mut output: Vec<Vec<StyledString>> = vec![];
        let mut styled_vec: Vec<StyledString> = vec![];

        // before we render, do a little patch-up work to support tabs
        self.copy_tabs(3);

        for (row, row_style) in self.text.iter().zip(&self.styles) {
            let mut current_style = Style::None;
            let mut current_text = String::new();

            for (&c, &s) in row.iter().zip(row_style) {
                if s != current_style {
                    if !current_text.is_empty() {
                        styled_vec.push(StyledString {
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
                styled_vec.push(StyledString {
                    text: current_text,
                    style: current_style,
                });
            }

            // We're done with the row, push and keep going
            output.push(styled_vec);

            styled_vec = vec![];
        }

        output
    }

    fn ensure_lines(&mut self, line: usize) {
        if self.text.len() <= line {
            self.text.resize(line + 1, Vec::new());
            self.styles.resize(line + 1, Vec::new());
        }
    }

    pub fn putc(&mut self, line: usize, col: usize, chr: char, style: Style) {
        self.ensure_lines(line);
        if col < self.text[line].len() {
            self.text[line][col] = chr;
            self.styles[line][col] = style;
        } else {
            let mut i = self.text[line].len();
            while i < col {
                self.text[line].push(' ');
                self.styles[line].push(Style::None);
                i += 1;
            }
            self.text[line].push(chr);
            self.styles[line].push(style);
        }
    }

    pub fn puts(&mut self, line: usize, col: usize, string: &str, style: Style) {
        for (c, n) in string.chars().zip(col..) {
            self.putc(line, n, c, style);
        }
    }

    pub fn prepend(&mut self, line: usize, string: &str, style: Style) {
        self.ensure_lines(line);

        // Push the old content over to make room for new content
        self.styles[line].splice(0..0, std::iter::repeat_n(Style::None, string.len()));
        self.text[line].splice(0..0, std::iter::repeat_n(' ', string.len()));

        self.puts(line, 0, string, style);
    }

    pub fn append(&mut self, line: usize, string: &str, style: Style) {
        let col = self.text.get(line).map_or(0, Vec::len);
        self.puts(line, col, string, style);
    }

    pub fn num_lines(&self) -> usize {
        self.text.len()
    }
}
