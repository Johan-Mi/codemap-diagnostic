// Copyright 2012-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::cmp::{Ordering, min};
use std::collections::HashMap;
use std::io::prelude::*;
use std::io::{self, IsTerminal};
use std::sync::Arc;
use termcolor::{Buffer, Color, WriteColor};
use termcolor::{BufferWriter, ColorChoice, ColorSpec};

use crate::snippet::{Annotation, AnnotationType, Line, MultilineAnnotation, Style, StyledString};
use crate::styled_buffer::StyledBuffer;
use crate::{Diagnostic, Level, SpanLabel, SpanStyle};
use codemap::{CodeMap, File};

/// Settings for terminal styling.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ColorConfig {
    /// Use colored output if stdout is a terminal.
    Auto,

    /// Always use colored output.
    Always,

    /// Never use colored output.
    Never,
}

/// Formats and prints diagnostic messages.
pub struct Emitter<'a> {
    dst: Destination<'a>,
    cm: Option<&'a CodeMap>,
}

struct FileWithAnnotatedLines {
    file: Arc<File>,
    lines: Vec<Line>,
    multiline_depth: usize,
}

impl<'a> Emitter<'a> {
    /// Creates an emitter wrapping stderr.
    #[must_use]
    pub fn stderr(color_config: ColorConfig, code_map: Option<&'a CodeMap>) -> Self {
        let choice = match color_config {
            ColorConfig::Always => ColorChoice::Always,
            ColorConfig::Auto if std::io::stderr().is_terminal() => ColorChoice::Auto,
            ColorConfig::Never | ColorConfig::Auto => ColorChoice::Never,
        };
        let dst = Destination::Buffered(BufferWriter::stderr(choice));
        Emitter { dst, cm: code_map }
    }

    /// Creates an emitter wrapping a vector.
    pub fn vec(vec: &'a mut Vec<u8>, code_map: Option<&'a CodeMap>) -> Self {
        Emitter {
            dst: Raw(Box::new(vec)),
            cm: code_map,
        }
    }

    /// Creates an emitter wrapping a boxed `Write` trait object.
    #[must_use]
    pub fn new(dst: Box<dyn Write + Send + 'a>, code_map: Option<&'a CodeMap>) -> Self {
        Emitter {
            dst: Raw(dst),
            cm: code_map,
        }
    }

    fn preprocess_annotations(
        cm: Option<&'a CodeMap>,
        spans: &[SpanLabel],
    ) -> Vec<FileWithAnnotatedLines> {
        let mut output = vec![];
        let mut multiline_annotations = vec![];

        if let Some(cm) = cm {
            for span_label in spans {
                let mut loc = cm.look_up_span(span_label.span);

                // Watch out for "empty spans". If we get a span like 6..6, we
                // want to just display a `^` at 6, so convert that to
                // 6..7. This is degenerate input, but it's best to degrade
                // gracefully -- and the parser likes to supply a span like
                // that for EOF, in particular.
                if loc.begin == loc.end {
                    loc.end.column = loc.begin.column + 1;
                }

                let ann_type = if loc.begin.line == loc.end.line {
                    AnnotationType::Singleline
                } else {
                    let ml = MultilineAnnotation {
                        depth: 1,
                        line_start: loc.begin.line,
                        line_end: loc.end.line,
                        start_col: loc.begin.column,
                        end_col: loc.end.column,
                        is_primary: span_label.style == SpanStyle::Primary,
                        label: span_label.label.clone(),
                    };
                    multiline_annotations.push((loc.file.clone(), ml.clone()));
                    AnnotationType::Multiline(ml)
                };
                let ann = Annotation {
                    start_col: loc.begin.column,
                    end_col: loc.end.column,
                    is_primary: span_label.style == SpanStyle::Primary,
                    label: span_label.label.clone(),
                    annotation_type: ann_type,
                };

                if !ann.is_multiline() {
                    add_annotation_to_file(&mut output, loc.file, loc.begin.line, ann);
                }
            }
        }

        // Find overlapping multiline annotations, put them at different depths
        multiline_annotations.sort_by_key(|a| (a.1.line_start, a.1.line_end));
        for (_, ann) in multiline_annotations.clone() {
            for (_, a) in &mut multiline_annotations {
                // Move all other multiline annotations overlapping with this one
                // one level to the right.
                if &ann != a
                    && num_overlap(ann.line_start, ann.line_end, a.line_start, a.line_end, true)
                {
                    a.depth += 1;
                } else {
                    break;
                }
            }
        }

        // max overlapping multiline spans
        let max_depth = multiline_annotations
            .iter()
            .map(|it| it.1.depth)
            .max()
            .unwrap_or(0);

        for (file, ann) in multiline_annotations {
            add_annotation_to_file(&mut output, file.clone(), ann.line_start, ann.as_start());
            let middle = min(ann.line_start + 4, ann.line_end);
            for line in ann.line_start + 1..middle {
                add_annotation_to_file(&mut output, file.clone(), line, ann.as_line());
            }
            if middle < ann.line_end - 1 {
                for line in ann.line_end - 1..ann.line_end {
                    add_annotation_to_file(&mut output, file.clone(), line, ann.as_line());
                }
            }
            add_annotation_to_file(&mut output, file, ann.line_end, ann.as_end());
        }
        for file_vec in &mut output {
            file_vec.multiline_depth = max_depth;
        }
        output
    }

    fn render_source_line(
        buffer: &mut StyledBuffer,
        file: &File,
        line: &Line,
        width_offset: usize,
        code_offset: usize,
    ) -> Vec<(usize, Style)> {
        let source_string = file.source_line(line.line_index);

        let line_offset = buffer.num_lines();

        // First create the source line we will highlight.
        buffer.puts(line_offset, code_offset, source_string, Style::Quotation);
        buffer.puts(
            line_offset,
            0,
            &((line.line_index + 1).to_string()),
            Style::LineNumber,
        );

        draw_col_separator(buffer, line_offset, width_offset - 2);

        // Special case when there's only one annotation involved, it is the start of a multiline
        // span and there's no text at the beginning of the code line. Instead of doing the whole
        // graph:
        //
        // 2 |   fn foo() {
        //   |  _^
        // 3 | |
        // 4 | | }
        //   | |_^ test
        //
        // we simplify the output to:
        //
        // 2 | / fn foo() {
        // 3 | |
        // 4 | | }
        //   | |_^ test
        if let [ann] = &*line.annotations
            && let AnnotationType::MultilineStart(depth) = ann.annotation_type
            && source_string
                .chars()
                .take(ann.start_col)
                .all(char::is_whitespace)
        {
            let style = if ann.is_primary {
                Style::UnderlinePrimary
            } else {
                Style::UnderlineSecondary
            };
            buffer.putc(line_offset, width_offset + depth - 1, '╭', style);
            return vec![(depth, style)];
        }

        // We want to display like this:
        //
        //      vec.push(vec.pop().unwrap());
        //      ---      ^^^               - previous borrow ends here
        //      |        |
        //      |        error occurs here
        //      previous borrow of `vec` occurs here
        //
        // But there are some weird edge cases to be aware of:
        //
        //      vec.push(vec.pop().unwrap());
        //      --------                    - previous borrow ends here
        //      ||
        //      |this makes no sense
        //      previous borrow of `vec` occurs here
        //
        // For this reason, we group the lines into "highlight lines"
        // and "annotations lines", where the highlight lines have the `^`.

        // Sort the annotations by (start, end col)
        // The labels are reversed, sort and then reversed again.
        // Consider a list of annotations (A1, A2, C1, C2, B1, B2) where
        // the letter signifies the span. Here we are only sorting by the
        // span and hence, the order of the elements with the same span will
        // not change. On reversing the ordering (|a, b| but b.cmp(a)), you get
        // (C1, C2, B1, B2, A1, A2). All the elements with the same span are
        // still ordered first to last, but all the elements with different
        // spans are ordered by their spans in last to first order. Last to
        // first order is important, because the jiggly lines and | are on
        // the left, so the rightmost span needs to be rendered first,
        // otherwise the lines would end up needing to go over a message.
        let mut annotations = line.annotations.clone();
        annotations.sort_by_key(|a| a.start_col);

        // First, figure out where each label will be positioned.
        //
        // In the case where you have the following annotations:
        //
        //      vec.push(vec.pop().unwrap());
        //      --------                    - previous borrow ends here [C]
        //      ||
        //      |this makes no sense [B]
        //      previous borrow of `vec` occurs here [A]
        //
        // `annotations_position` will hold [(2, A), (1, B), (0, C)].
        //
        // We try, when possible, to stick the rightmost annotation at the end
        // of the highlight line:
        //
        //      vec.push(vec.pop().unwrap());
        //      ---      ---               - previous borrow ends here
        //
        // But sometimes that's not possible because one of the other
        // annotations overlaps it. For example, from the test
        // `span_overlap_label`, we have the following annotations
        // (written on distinct lines for clarity):
        //
        //      fn foo(x: u32) {
        //      --------------
        //             -
        //
        // In this case, we can't stick the rightmost-most label on
        // the highlight line, or we would get:
        //
        //      fn foo(x: u32) {
        //      -------- x_span
        //      |
        //      fn_span
        //
        // which is totally weird. Instead we want:
        //
        //      fn foo(x: u32) {
        //      --------------
        //      |      |
        //      |      x_span
        //      fn_span
        //
        // which is...less weird, at least. In fact, in general, if
        // the rightmost span overlaps with any other span, we should
        // use the "hang below" version, so we can at least make it
        // clear where the span *starts*. There's an exception for this
        // logic, when the labels do not have a message:
        //
        //      fn foo(x: u32) {
        //      --------------
        //             |
        //             x_span
        //
        // instead of:
        //
        //      fn foo(x: u32) {
        //      --------------
        //      |      |
        //      |      x_span
        //      <EMPTY LINE>
        //
        let mut annotations_position = vec![];
        let mut line_len = 0;
        let mut p = 0;
        for (i, annotation) in annotations.iter().enumerate() {
            for (j, next) in annotations.iter().enumerate() {
                if overlaps(next, annotation, 0)  // This label overlaps with another one and both
                    && annotation.has_label()     // take space (they have text and are not
                    && j > i                      // multiline lines).
                    && p == 0
                // We're currently on the first line, move the label one line down
                {
                    // This annotation needs a new line in the output.
                    p += 1;
                    break;
                }
            }
            annotations_position.push((p, annotation));
            for (j, next) in annotations.iter().enumerate() {
                if j > i {
                    let l = next.label.as_ref().map_or(0, |label| label.len() + 2);
                    if (overlaps(next, annotation, l) // Do not allow two labels to be in the same
                                                     // line if they overlap including padding, to
                                                     // avoid situations like:
                                                     //
                                                     //      fn foo(x: u32) {
                                                     //      -------^------
                                                     //      |      |
                                                     //      fn_spanx_span
                                                     //
                        && annotation.has_label()    // Both labels must have some text, otherwise
                        && next.has_label())         // they are not overlapping.
                                                     // Do not add a new line if this annotation
                                                     // or the next are vertical line placeholders.
                        || (annotation.takes_space() // If either this or the next annotation is
                            && next.has_label())     // multiline start/end, move it to a new line
                        || (annotation.has_label()   // so as not to overlap the orizontal lines.
                            && next.takes_space())
                        || (annotation.takes_space() && next.takes_space())
                        || (overlaps(next, annotation, l)
                            && next.end_col <= annotation.end_col
                            && next.has_label()
                            && p == 0)
                    // Avoid #42595.
                    {
                        // This annotation needs a new line in the output.
                        p += 1;
                        break;
                    }
                }
            }
            if line_len < p {
                line_len = p;
            }
        }

        if line_len != 0 {
            line_len += 1;
        }

        // If there are no annotations or the only annotations on this line are
        // MultilineLine, then there's only code being shown, stop processing.
        if line.annotations.iter().all(Annotation::is_line) {
            return vec![];
        }

        // Write the colunmn separator.
        //
        // After this we will have:
        //
        // 2 |   fn foo() {
        //   |
        //   |
        //   |
        // 3 |
        // 4 |   }
        //   |
        for pos in 0..=line_len {
            draw_col_separator(buffer, line_offset + pos + 1, width_offset - 2);
            buffer.putc(
                line_offset + pos + 1,
                width_offset - 2,
                '│',
                Style::LineNumber,
            );
        }

        // Write the horizontal lines for multiline annotations
        // (only the first and last lines need this).
        //
        // After this we will have:
        //
        // 2 |   fn foo() {
        //   |  __________
        //   |
        //   |
        // 3 |
        // 4 |   }
        //   |  _
        for &(pos, annotation) in &annotations_position {
            let style = if annotation.is_primary {
                Style::UnderlinePrimary
            } else {
                Style::UnderlineSecondary
            };
            let pos = pos + 1;
            if let AnnotationType::MultilineStart(depth) | AnnotationType::MultilineEnd(depth) =
                annotation.annotation_type
            {
                draw_range(
                    buffer,
                    '─',
                    line_offset + pos,
                    width_offset + depth,
                    code_offset + annotation.start_col,
                    style,
                );
            }
        }

        // Write the vertical lines for labels that are on a different line as the underline.
        //
        // After this we will have:
        //
        // 2 |   fn foo() {
        //   |  __________
        //   | |    |
        //   | |
        // 3 |
        // 4 | | }
        //   | |_
        for &(pos, annotation) in &annotations_position {
            let style = if annotation.is_primary {
                Style::UnderlinePrimary
            } else {
                Style::UnderlineSecondary
            };
            let pos = pos + 1;

            if pos > 1 && (annotation.has_label() || annotation.takes_space()) {
                for p in (line_offset + 1)..=(line_offset + pos) {
                    buffer.putc(p, code_offset + annotation.start_col, '│', style);
                }
            }
            match annotation.annotation_type {
                AnnotationType::MultilineStart(depth) => {
                    buffer.putc(line_offset + pos, width_offset + depth - 1, '╭', style);
                    for p in line_offset + pos + 1..line_offset + line_len + 2 {
                        buffer.putc(p, width_offset + depth - 1, '│', style);
                    }
                }
                AnnotationType::MultilineEnd(depth) => {
                    for p in line_offset..(line_offset + pos) {
                        buffer.putc(p, width_offset + depth - 1, '│', style);
                    }
                    buffer.putc(line_offset + pos, width_offset + depth - 1, '╰', style);
                }
                _ => (),
            }
        }

        // Write the labels on the annotations that actually have a label.
        //
        // After this we will have:
        //
        // 2 |   fn foo() {
        //   |  __________
        //   |      |
        //   |      something about `foo`
        // 3 |
        // 4 |   }
        //   |  _  test
        for &(pos, annotation) in &annotations_position {
            let style = if annotation.is_primary {
                Style::LabelPrimary
            } else {
                Style::LabelSecondary
            };
            let (pos, col) = if pos == 0 {
                (pos + 1, annotation.end_col + 1)
            } else {
                (pos + 2, annotation.start_col)
            };
            if let Some(label) = &annotation.label {
                buffer.puts(line_offset + pos, code_offset + col, label, style);
            }
        }

        // Sort from biggest span to smallest span so that smaller spans are
        // represented in the output:
        //
        // x | fn foo()
        //   | ^^^---^^
        //   | |  |
        //   | |  something about `foo`
        //   | something about `fn foo()`
        annotations_position.sort_by_key(|a| std::cmp::Reverse(a.1.len()));

        // Write the underlines.
        //
        // After this we will have:
        //
        // 2 |   fn foo() {
        //   |  ____-_____^
        //   |      |
        //   |      something about `foo`
        // 3 |
        // 4 |   }
        //   |  _^  test
        for &(_, annotation) in &annotations_position {
            let (underline, style) = if annotation.is_primary {
                ('^', Style::UnderlinePrimary)
            } else {
                ('-', Style::UnderlineSecondary)
            };
            for p in annotation.start_col..annotation.end_col {
                buffer.putc(line_offset + 1, code_offset + p, underline, style);
            }
        }
        annotations_position
            .iter()
            .filter_map(|&(_, annotation)| match annotation.annotation_type {
                AnnotationType::MultilineStart(p) | AnnotationType::MultilineEnd(p) => {
                    let style = if annotation.is_primary {
                        Style::LabelPrimary
                    } else {
                        Style::LabelSecondary
                    };
                    Some((p, style))
                }
                _ => None,
            })
            .collect::<Vec<_>>()
    }

    fn get_max_line_num(&self, diagnostics: &[Diagnostic]) -> usize {
        self.cm
            .and_then(|cm| {
                diagnostics
                    .iter()
                    .flat_map(|d| &d.spans)
                    .map(|span_label| cm.look_up_pos(span_label.span.high()).position.line)
                    .max()
            })
            .unwrap_or(0)
    }

    /// Add a left margin to every line but the first, given a padding length and the label being
    /// displayed, keeping the provided highlighting.
    fn msg_to_buffer(
        buffer: &mut StyledBuffer,
        msg: &[(&str, Style)],
        padding: usize,
        label: &str,
        override_style: Option<Style>,
    ) {
        /// Return wether `style`, or the override if present and the style is `NoStyle`.
        fn style_or_override(style: Style, override_style: Option<Style>) -> Style {
            if let Some(o) = override_style
                && style == Style::NoStyle
            {
                return o;
            }
            style
        }

        // The extra 5 ` ` is padding that's always needed to align to the `note: `:
        //
        //   error: message
        //     --> file.rs:13:20
        //      |
        //   13 |     <CODE>
        //      |      ^^^^
        //      |
        //      = note: multiline
        //              message
        //   ++^^^----xx
        //    |  |   | |
        //    |  |   | magic `2`
        //    |  |   length of label
        //    |  magic `3`
        //    `max_line_num_len`
        let padding = " ".repeat(padding + label.len() + 5);

        let mut line_number = 0;

        // Provided the following diagnostic message:
        //
        //     let msg = vec![
        //       ("
        //       ("highlighted multiline\nstring to\nsee how it ", Style::NoStyle),
        //       ("looks", Style::Highlight),
        //       ("with\nvery ", Style::NoStyle),
        //       ("weird", Style::Highlight),
        //       (" formats\n", Style::NoStyle),
        //       ("see?", Style::Highlight),
        //     ];
        //
        // the expected output on a note is (* surround the  highlighted text)
        //
        //        = note: highlighted multiline
        //                string to
        //                see how it *looks* with
        //                very *weird* formats
        //                see?
        for (text, style) in msg {
            let lines = text.split('\n').collect::<Vec<_>>();
            if lines.len() > 1 {
                for (i, line) in lines.iter().enumerate() {
                    if i != 0 {
                        line_number += 1;
                        buffer.append(line_number, &padding, Style::NoStyle);
                    }
                    buffer.append(line_number, line, style_or_override(*style, override_style));
                }
            } else {
                buffer.append(line_number, text, style_or_override(*style, override_style));
            }
        }
    }

    fn emit_message_default(
        &mut self,
        spans: &[SpanLabel],
        msg: &[(&str, Style)],
        code: Option<&str>,
        level: Level,
        max_line_num_len: usize,
        is_secondary: bool,
    ) -> io::Result<()> {
        let mut buffer = StyledBuffer::default();

        if is_secondary && spans.is_empty() {
            // This is a secondary message with no span info
            for _ in 0..max_line_num_len {
                buffer.prepend(0, " ", Style::NoStyle);
            }
            draw_note_separator(&mut buffer, 0, max_line_num_len + 1);
            buffer.append(0, &level.to_string(), Style::HeaderMsg);
            buffer.append(0, ": ", Style::NoStyle);
            Self::msg_to_buffer(&mut buffer, msg, max_line_num_len, "note", None);
        } else {
            buffer.append(0, &level.to_string(), Style::Level(level));
            if let Some(code) = code {
                buffer.append(0, "[", Style::Level(level));
                buffer.append(0, code, Style::Level(level));
                buffer.append(0, "]", Style::Level(level));
            }
            buffer.append(0, ": ", Style::HeaderMsg);
            for (text, _) in msg {
                buffer.append(0, text, Style::HeaderMsg);
            }
        }

        // Preprocess all the annotations so that they are grouped by file and by line number
        // This helps us quickly iterate over the whole message (including secondary file spans)
        let mut annotated_files = Emitter::preprocess_annotations(self.cm, spans);

        // Make sure our primary file comes first
        let primary_lo = if let (Some(cm), Some(primary_span)) = (
            self.cm.as_ref(),
            spans.iter().find(|x| x.style == SpanStyle::Primary),
        ) {
            cm.look_up_pos(primary_span.span.low())
        } else {
            // If we don't have span information, emit and exit
            emit_to_destination(&buffer.render(), level, &mut self.dst)?;
            return Ok(());
        };
        if let Ok(pos) =
            annotated_files.binary_search_by(|x| x.file.name().cmp(primary_lo.file.name()))
        {
            annotated_files.swap(0, pos);
        }

        // Print out the annotate source lines that correspond with the error
        for annotated_file in annotated_files {
            // remember where we are in the output buffer for easy reference
            let buffer_msg_line_offset = buffer.num_lines();

            // print out the span location and spacer before we print the annotated source
            // to do this, we need to know if this span will be primary
            let is_primary = Arc::ptr_eq(&primary_lo.file, &annotated_file.file);
            if is_primary {
                buffer.prepend(buffer_msg_line_offset, "--> ", Style::LineNumber);
                let loc = primary_lo.clone();
                buffer.append(
                    buffer_msg_line_offset,
                    &format!(
                        "{}:{}:{}",
                        loc.file.name(),
                        loc.position.line + 1,
                        loc.position.column + 1
                    ),
                    Style::LineAndColumn,
                );
                for _ in 0..max_line_num_len {
                    buffer.prepend(buffer_msg_line_offset, " ", Style::NoStyle);
                }
            } else {
                // Add spacing line
                draw_col_separator(&mut buffer, buffer_msg_line_offset, max_line_num_len + 1);

                // Then, the secondary file indicator
                buffer.prepend(buffer_msg_line_offset + 1, "::: ", Style::LineNumber);
                buffer.append(
                    buffer_msg_line_offset + 1,
                    annotated_file.file.name(),
                    Style::LineAndColumn,
                );
                for _ in 0..max_line_num_len {
                    buffer.prepend(buffer_msg_line_offset + 1, " ", Style::NoStyle);
                }
            }

            // Put in the spacer between the location and annotated source
            let buffer_msg_line_offset = buffer.num_lines();
            draw_col_separator_no_space(&mut buffer, buffer_msg_line_offset, max_line_num_len + 1);

            // Contains the vertical lines' positions for active multiline annotations
            let mut multilines = HashMap::new();

            // Next, output the annotate source for this file
            for line_idx in 0..annotated_file.lines.len() {
                let previous_buffer_line = buffer.num_lines();

                let width_offset = 3 + max_line_num_len;
                let code_offset = if annotated_file.multiline_depth == 0 {
                    width_offset
                } else {
                    width_offset + annotated_file.multiline_depth + 1
                };

                let depths = Self::render_source_line(
                    &mut buffer,
                    &annotated_file.file,
                    &annotated_file.lines[line_idx],
                    width_offset,
                    code_offset,
                );

                let mut to_add = HashMap::new();

                for (depth, style) in depths {
                    if multilines.remove(&depth).is_none() {
                        to_add.insert(depth, style);
                    }
                }

                // Set the multiline annotation vertical lines to the left of
                // the code in this line.
                for (depth, style) in &multilines {
                    for line in previous_buffer_line..buffer.num_lines() {
                        draw_multiline_line(&mut buffer, line, width_offset, *depth, *style);
                    }
                }
                // check to see if we need to print out or elide lines that come between
                // this annotated line and the next one.
                if line_idx < (annotated_file.lines.len() - 1) {
                    let line_idx_delta = annotated_file.lines[line_idx + 1].line_index
                        - annotated_file.lines[line_idx].line_index;
                    match line_idx_delta.cmp(&2) {
                        Ordering::Less => {}
                        Ordering::Equal => {
                            let unannotated_line = annotated_file
                                .file
                                .source_line(annotated_file.lines[line_idx].line_index);

                            let last_buffer_line_num = buffer.num_lines();

                            buffer.puts(
                                last_buffer_line_num,
                                0,
                                &(annotated_file.lines[line_idx + 1].line_index - 1).to_string(),
                                Style::LineNumber,
                            );
                            draw_col_separator(
                                &mut buffer,
                                last_buffer_line_num,
                                1 + max_line_num_len,
                            );
                            buffer.puts(
                                last_buffer_line_num,
                                code_offset,
                                unannotated_line,
                                Style::Quotation,
                            );

                            for (depth, style) in &multilines {
                                draw_multiline_line(
                                    &mut buffer,
                                    last_buffer_line_num,
                                    width_offset,
                                    *depth,
                                    *style,
                                );
                            }
                        }
                        Ordering::Greater => {
                            let last_buffer_line_num = buffer.num_lines();
                            buffer.puts(last_buffer_line_num, 0, "...", Style::LineNumber);

                            // Set the multiline annotation vertical lines on `...` bridging line.
                            for (depth, style) in &multilines {
                                draw_multiline_line(
                                    &mut buffer,
                                    last_buffer_line_num,
                                    width_offset,
                                    *depth,
                                    *style,
                                );
                            }
                        }
                    }
                }

                multilines.extend(&to_add);
            }
        }

        // final step: take our styled buffer, render it, then output it
        emit_to_destination(&buffer.render(), level, &mut self.dst)?;

        Ok(())
    }

    /// Print a group of diagnostic messages.
    ///
    /// The messages within a group are printed atomically without spacing between them, and share
    /// consistent formatting elements, such as aligned line number width.
    pub fn emit(&mut self, msgs: &[Diagnostic]) {
        let max_line_num = self.get_max_line_num(msgs) + 1;
        let max_line_num_len = max_line_num.to_string().len();

        for msg in msgs {
            if let Err(e) = self.emit_message_default(
                &msg.spans[..],
                &[(&msg.message, Style::NoStyle)],
                msg.code.as_deref(),
                msg.level,
                max_line_num_len,
                false,
            ) {
                panic!("failed to emit error: {e}");
            }
        }

        let mut dst = self.dst.writable();
        if let Err(e) = writeln!(dst) {
            panic!("failed to emit error: {e}");
        }
        if let Err(e) = dst.flush() {
            panic!("failed to emit error: {e}");
        }
    }
}

fn add_annotation_to_file(
    file_vec: &mut Vec<FileWithAnnotatedLines>,
    file: Arc<File>,
    line_index: usize,
    ann: Annotation,
) {
    // Look through each of our files for the one we're adding to
    if let Some(slot) = file_vec.iter_mut().find(|it| Arc::ptr_eq(&it.file, &file)) {
        // See if we already have a line for it
        if let Some(line_slot) = slot.lines.iter_mut().find(|it| it.line_index == line_index) {
            line_slot.annotations.push(ann);
        } else {
            // We don't have a line yet, create one
            slot.lines.push(Line {
                line_index,
                annotations: vec![ann],
            });
            slot.lines.sort();
        }
    } else {
        // This is the first time we're seeing the file
        file_vec.push(FileWithAnnotatedLines {
            file,
            lines: vec![Line {
                line_index,
                annotations: vec![ann],
            }],
            multiline_depth: 0,
        });
    }
}

fn draw_col_separator(buffer: &mut StyledBuffer, line: usize, col: usize) {
    buffer.puts(line, col, "│ ", Style::LineNumber);
}

fn draw_col_separator_no_space(buffer: &mut StyledBuffer, line: usize, col: usize) {
    buffer.putc(line, col, '│', Style::LineNumber);
}

fn draw_range(
    buffer: &mut StyledBuffer,
    symbol: char,
    line: usize,
    col_from: usize,
    col_to: usize,
    style: Style,
) {
    for col in col_from..col_to {
        buffer.putc(line, col, symbol, style);
    }
}

fn draw_note_separator(buffer: &mut StyledBuffer, line: usize, col: usize) {
    buffer.puts(line, col, "= ", Style::LineNumber);
}

fn draw_multiline_line(
    buffer: &mut StyledBuffer,
    line: usize,
    offset: usize,
    depth: usize,
    style: Style,
) {
    buffer.putc(line, offset + depth - 1, '│', style);
}

fn num_overlap(
    a_start: usize,
    a_end: usize,
    b_start: usize,
    b_end: usize,
    inclusive: bool,
) -> bool {
    let extra = usize::from(inclusive);
    (b_start..b_end + extra).contains(&a_start) || (a_start..a_end + extra).contains(&b_start)
}
fn overlaps(a1: &Annotation, a2: &Annotation, padding: usize) -> bool {
    num_overlap(
        a1.start_col,
        a1.end_col + padding,
        a2.start_col,
        a2.end_col,
        false,
    )
}

fn emit_to_destination(
    rendered_buffer: &[Vec<StyledString>],
    lvl: Level,
    dst: &mut Destination,
) -> io::Result<()> {
    let mut dst = dst.writable();

    for line in rendered_buffer {
        for part in line {
            dst.apply_style(lvl, part.style)?;
            write!(dst, "{}", part.text)?;
            if let WritableDst::Buffered(_, t) = &mut dst {
                t.reset()?;
            }
        }
        writeln!(dst)?;
    }
    dst.flush()?;
    Ok(())
}

enum Destination<'a> {
    Buffered(BufferWriter),
    Raw(Box<dyn Write + Send + 'a>),
}

use self::Destination::Raw;

enum WritableDst<'a, 'b> {
    Buffered(&'b mut BufferWriter, Buffer),
    Raw(&'b mut Box<dyn Write + Send + 'a>),
}

impl<'a> Destination<'a> {
    fn writable<'b>(&'b mut self) -> WritableDst<'a, 'b> {
        match self {
            Destination::Buffered(t) => {
                let buf = t.buffer();
                WritableDst::Buffered(t, buf)
            }
            Destination::Raw(t) => WritableDst::Raw(t),
        }
    }
}

impl WritableDst<'_, '_> {
    fn apply_style(&mut self, lvl: Level, style: Style) -> io::Result<()> {
        let WritableDst::Buffered(_, t) = self else {
            return Ok(());
        };

        let mut spec = ColorSpec::new();
        match style {
            Style::LineAndColumn | Style::Quotation | Style::NoStyle => {}
            Style::LineNumber | Style::UnderlineSecondary | Style::LabelSecondary => {
                spec.set_bold(true).set_intense(true);
                spec.set_fg(Some(if cfg!(windows) {
                    Color::Cyan
                } else {
                    Color::Blue
                }));
            }
            Style::HeaderMsg => {
                spec.set_bold(true);
                if cfg!(windows) {
                    spec.set_intense(true).set_fg(Some(Color::White));
                }
            }
            Style::UnderlinePrimary | Style::LabelPrimary => {
                spec = lvl.color();
                spec.set_bold(true);
            }
            Style::Level(lvl) => {
                spec = lvl.color();
                spec.set_bold(true);
            }
        }
        t.set_color(&spec)
    }
}

impl Write for WritableDst<'_, '_> {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        match self {
            WritableDst::Buffered(_, buf) => buf.write(bytes),
            WritableDst::Raw(w) => w.write(bytes),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self {
            WritableDst::Buffered(_, buf) => buf.flush(),
            WritableDst::Raw(w) => w.flush(),
        }
    }
}

impl Drop for WritableDst<'_, '_> {
    fn drop(&mut self) {
        if let WritableDst::Buffered(dst, buf) = self {
            drop(dst.print(buf));
        }
    }
}
