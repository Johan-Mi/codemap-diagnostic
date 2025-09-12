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
        let Some(([.., row], rest)) = self.text.split_at_mut_checked(row + 1) else {
            return;
        };
        for t in rest {
            for (_, b) in std::iter::zip(&*row, t).filter(|(a, b)| **a == '\t' && **b == ' ') {
                *b = '\t';
            }
        }
    }

    pub fn render(&mut self) -> impl Iterator<Item = StyledString> {
        // before we render, do a little patch-up work to support tabs
        self.copy_tabs(3);

        self.text
            .iter()
            .zip(&self.styles)
            .flat_map(|(row, row_style)| {
                let mut row = row.iter();
                row_style
                    .chunk_by(PartialEq::eq)
                    .map(move |chunk| StyledString {
                        text: row.by_ref().take(chunk.len()).collect(),
                        style: chunk[0],
                    })
                    .chain(std::iter::once(StyledString {
                        text: "\n".to_owned(),
                        style: Style::None,
                    }))
            })
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
