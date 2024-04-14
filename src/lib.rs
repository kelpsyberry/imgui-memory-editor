macro_rules! style {
    ($ui: expr, $ident: ident) => {
        // SAFETY: The &Style is immediately dropped
        unsafe { $ui.style() }.$ident
    };
}

mod range_inclusive;
pub use range_inclusive::RangeInclusive;
mod scrollbar;
mod y_pos;

use core::{fmt::Write, iter::once, mem::size_of, num::NonZeroU8};
use imgui::{
    color::ImColor32, Drag, Key, MouseButton, StyleColor, StyleVar, Ui, WindowHoveredFlags,
};
use scrollbar::Scrollbar;
use y_pos::{YPos, YPosRaw};

// TODO:
// - Add an `access_rights` callback that returns whether a given address's access rights are
//  `None`, `ReadOnly` or `ReadWrite`.
// - Add editing... somehow

macro_rules! str_buf {
    ($buf: expr, $($args: tt)*) => {{
        use ::core::fmt::Write;
        $buf.clear();
        let _ = write!($buf, $($args)*);
        &$buf
    }};
}

bitflags::bitflags! {
    pub struct Flags: u16 {
        // Editor properties
        const READ_ONLY = 1 << 0;
        const SHOW_VIEW_OPTIONS = 1 << 1;
        const SHOW_RANGE = 1 << 2;

        // View options
        const GRAY_OUT_ZEROS = 1 << 8;
        const UPPERCASE_HEX = 1 << 9;
        const LITTLE_ENDIAN_COLS = 1 << 10;
        const SHOW_HEXII = 1 << 11;
        const SHOW_ASCII = 1 << 12;
        const SHOW_DATA_PREVIEW = 1 << 13;
    }
}

impl Flags {
    const DATA_LAYOUT_MASK: Self = Self::from_bits_truncate(Flags::SHOW_ASCII.bits());

    const FOOTER_LAYOUT_MASK: Self = Self::from_bits_truncate(
        Flags::SHOW_VIEW_OPTIONS.bits()
            | Flags::SHOW_RANGE.bits()
            | Flags::UPPERCASE_HEX.bits()
            | Flags::SHOW_DATA_PREVIEW.bits(),
    );
}

pub type Addr = u64;
type WAddr = u128;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DataPreviewBaseTy {
    I8,
    I16,
    I24,
    I32,
    I64,
    I128,
    F32,
    F64,
    C8,
    C16,
    C32,
}

impl DataPreviewBaseTy {
    fn allows_signed(self) -> bool {
        matches!(
            self,
            DataPreviewBaseTy::I8
                | DataPreviewBaseTy::I16
                | DataPreviewBaseTy::I24
                | DataPreviewBaseTy::I32
                | DataPreviewBaseTy::I64
                | DataPreviewBaseTy::I128
        )
    }

    fn allows_endianness(self) -> bool {
        !matches!(self, DataPreviewBaseTy::I8 | DataPreviewBaseTy::C8)
    }
}

impl AsRef<str> for DataPreviewBaseTy {
    fn as_ref(&self) -> &str {
        match self {
            DataPreviewBaseTy::I8 => "Int8",
            DataPreviewBaseTy::I16 => "Int16",
            DataPreviewBaseTy::I24 => "Int24",
            DataPreviewBaseTy::I32 => "Int32",
            DataPreviewBaseTy::I64 => "Int64",
            DataPreviewBaseTy::I128 => "Int128",
            DataPreviewBaseTy::F32 => "Float32",
            DataPreviewBaseTy::F64 => "Float64",
            DataPreviewBaseTy::C8 => "UTF-8",
            DataPreviewBaseTy::C16 => "UTF-16",
            DataPreviewBaseTy::C32 => "UTF-32",
        }
    }
}

struct DataPreviewTy {
    base: DataPreviewBaseTy,
    is_signed: bool,
    is_little_endian: bool,
}

#[derive(Clone, Copy, Debug)]
pub enum DisplayMode<'a> {
    Window { title: &'a str },
    Child { height: f32 },
}

struct SelectedAddr {
    addr: Addr,
    input_buffer: String,
    was_changed: bool,
    requires_focus: bool,
}

impl SelectedAddr {
    fn set_range(&mut self, range: &RangeInclusive<Addr>) {
        self.set(self.addr, false, range);
    }

    fn set(&mut self, addr: Addr, should_focus: bool, range: &RangeInclusive<Addr>) {
        let clamped = addr.clamp(range.start, range.end);
        if clamped != self.addr {
            self.addr = clamped;
            self.was_changed = true;
        }
        self.requires_focus |= should_focus;
    }
}

pub struct MemoryEditor {
    cols: NonZeroU8,
    col_size: NonZeroU8,
    addr_digits: Option<NonZeroU8>,
    addr_range: RangeInclusive<Addr>,
    flags: Flags,

    scrollbar: Scrollbar,
    visible_data_rows: RangeInclusive<Addr>,

    selected_addr: SelectedAddr,

    data_preview_ty: DataPreviewTy,

    str_buffer: String,
    data_layout: Option<DataLayout>,
    footer_layout: Option<FooterLayout>,

    pub highlights: Vec<(RangeInclusive<Addr>, ImColor32)>,
}

impl MemoryEditor {
    #[inline]
    pub fn new() -> Self {
        MemoryEditor {
            cols: NonZeroU8::new(16).unwrap(),
            col_size: NonZeroU8::new(1).unwrap(),
            addr_digits: None,
            addr_range: (0, 0).into(),
            flags: Flags::SHOW_VIEW_OPTIONS
                | Flags::SHOW_RANGE
                | Flags::GRAY_OUT_ZEROS
                | Flags::UPPERCASE_HEX
                | Flags::SHOW_ASCII,

            scrollbar: Scrollbar::new(),
            visible_data_rows: (0, 0).into(),

            selected_addr: SelectedAddr {
                addr: 0,
                input_buffer: String::new(),
                was_changed: true,
                requires_focus: false,
            },

            data_preview_ty: DataPreviewTy {
                base: DataPreviewBaseTy::I8,
                is_signed: false,
                is_little_endian: true,
            },

            str_buffer: String::new(),
            data_layout: None,
            footer_layout: None,

            highlights: Vec::new(),
        }
    }

    #[inline]
    pub fn set_cols(&mut self, value: NonZeroU8) {
        self.cols = value;
        self.data_layout = None;
    }

    #[inline]
    pub fn set_col_size(&mut self, value: NonZeroU8) {
        self.col_size = value;
        self.data_layout = None;
    }

    #[inline]
    pub fn set_addr_digits(&mut self, value: Option<NonZeroU8>) {
        self.addr_digits = value;
        self.data_layout = None;
        self.footer_layout = None;
    }

    #[inline]
    pub fn set_addr_range(&mut self, value: RangeInclusive<Addr>) {
        self.addr_range = value;
        self.selected_addr.set_range(&value);
        self.data_layout = None;
        self.footer_layout = None;
    }

    #[inline]
    pub fn set_flags(&mut self, flags: Flags) {
        if flags
            .symmetric_difference(self.flags)
            .intersects(Flags::DATA_LAYOUT_MASK)
        {
            self.data_layout = None;
        }
        if flags
            .symmetric_difference(self.flags)
            .intersects(Flags::FOOTER_LAYOUT_MASK)
        {
            self.footer_layout = None;
        }
        self.flags = flags;
    }

    #[inline]
    pub fn set_read_only(&mut self, value: bool) {
        self.flags.set(Flags::READ_ONLY, value);
    }

    #[inline]
    pub fn set_show_view_options(&mut self, value: bool) {
        self.flags.set(Flags::SHOW_VIEW_OPTIONS, value);
        self.footer_layout = None;
    }

    #[inline]
    pub fn set_show_range(&mut self, value: bool) {
        self.flags.set(Flags::SHOW_RANGE, value);
        self.footer_layout = None;
    }

    #[inline]
    pub fn set_gray_out_zeros(&mut self, value: bool) {
        self.flags.set(Flags::GRAY_OUT_ZEROS, value);
    }

    #[inline]
    pub fn set_uppercase_hex(&mut self, value: bool) {
        self.flags.set(Flags::UPPERCASE_HEX, value);
    }

    #[inline]
    pub fn set_little_endian_cols(&mut self, value: bool) {
        self.flags.set(Flags::LITTLE_ENDIAN_COLS, value);
    }

    #[inline]
    pub fn set_show_hexii(&mut self, value: bool) {
        self.flags.set(Flags::SHOW_HEXII, value);
    }

    #[inline]
    pub fn set_show_ascii(&mut self, value: bool) {
        self.flags.set(Flags::SHOW_ASCII, value);
        self.data_layout = None;
    }

    #[inline]
    pub fn set_show_data_preview(&mut self, value: bool) {
        self.flags.set(Flags::SHOW_DATA_PREVIEW, value);
        self.footer_layout = None;
    }

    #[inline]
    pub fn set_selected_addr(&mut self, addr: Addr, should_focus: bool) {
        self.selected_addr.set(addr, should_focus, &self.addr_range)
    }
}

impl Default for MemoryEditor {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
struct DataLayout {
    bytes_per_row: Addr,

    addr_digits: NonZeroU8,
    total_rows: WAddr,

    row_spacing: f32,
    row_height_fixed: YPos,
    row_height: f32,
    height_fixed: YPos,

    glyph_width: f32,
    hex_byte_width: f32,

    col_spacing: f32,
    hex_col_width: f32,
    ascii_col_width: f32,

    hex_start_win_x: f32,
    hex_end_win_x: f32,
    ascii_sep_win_x: f32,
    ascii_start_win_x: f32,
    total_width: f32,
}

#[derive(Clone)]
struct FooterLayout {
    win_width: f32,
    range_text: String,
    range_width: f32,
    addr_input_width: f32,
    controls_and_separator_height: f32,
    height: f32,
}

macro_rules! data_layout {
    ($self: expr, $ui: expr) => {{
        if $self.data_layout.is_none() {
            $self.data_layout = Some($self.compute_data_layout($ui));
        }
        // SAFETY: data_layout is always Some(_) after calling compute_data_layout
        unsafe { $self.data_layout.as_ref().unwrap_unchecked() }
    }};
}

macro_rules! footer_layout {
    ($self: expr, $ui: expr, $data_layout: expr) => {{
        if $self.footer_layout.is_none() {
            $self.footer_layout = Some($self.compute_footer_layout($ui, $data_layout));
        }
        // SAFETY: footer_layout is always Some(_) after calling compute_footer_layout
        unsafe { $self.footer_layout.as_ref().unwrap_unchecked() }
    }};
}

const SEPARATOR_WIDTH: f32 = 1.0;

impl MemoryEditor {
    #[inline]
    pub fn visible_addrs(&mut self, context_rows: Addr, ui: &Ui) -> RangeInclusive<Addr> {
        let data_layout = data_layout!(self, ui);

        (
            self.addr_range.start
                + self.visible_data_rows.start.saturating_sub(context_rows)
                    * data_layout.bytes_per_row,
            self.addr_range
                .start
                .saturating_add(
                    self.visible_data_rows
                        .end
                        .saturating_add(context_rows)
                        .saturating_mul(data_layout.bytes_per_row)
                        .saturating_add(data_layout.bytes_per_row - 1),
                )
                .min(self.addr_range.end),
        )
            .into()
    }

    fn compute_data_layout(&self, ui: &Ui) -> DataLayout {
        let bytes_per_row = self.cols.get() as Addr * self.col_size.get() as Addr;

        let addr_digits = self.addr_digits.unwrap_or_else(|| {
            NonZeroU8::new(
                ((Addr::BITS + 3 - self.addr_range.end.leading_zeros()) >> 2).max(1) as u8,
            )
            .unwrap()
        });
        let addr_width = ui.calc_text_size(format!(
            "{:0addr_digits$X}:",
            0,
            addr_digits = addr_digits.get() as usize
        ))[0];

        let total_rows =
            ((self.addr_range.end - self.addr_range.start) / bytes_per_row) as WAddr + 1;

        // SAFETY: This code doesn't modify the style
        let style = unsafe { ui.style() };

        let row_spacing = style.item_spacing[1];
        let row_height_fixed = YPos::from(ui.text_line_height() + row_spacing);
        let row_height = row_height_fixed.into();

        let height_fixed = row_height_fixed * total_rows as YPosRaw;

        let glyph_width = ui.calc_text_size("0")[0];
        let hex_byte_width = ui.calc_text_size("00")[0];

        let col_spacing = style.item_spacing[0];
        let separator_spacing = style.cell_padding[0].max(col_spacing);
        let hex_col_width = hex_byte_width * self.col_size.get() as f32 + col_spacing;
        let ascii_col_width = glyph_width * self.col_size.get() as f32 + col_spacing;

        let hex_start_win_x = addr_width + separator_spacing - col_spacing * 0.5;
        let hex_end_win_x = hex_start_win_x + hex_col_width * self.cols.get() as f32;
        let (ascii_sep_win_x, ascii_start_win_x, total_width) =
            if self.flags.contains(Flags::SHOW_ASCII) {
                let ascii_spacer_width = separator_spacing - col_spacing + SEPARATOR_WIDTH;
                let ascii_start_win_x = hex_end_win_x + ascii_spacer_width;
                (
                    hex_end_win_x + ascii_spacer_width * 0.5 - 0.5,
                    ascii_start_win_x,
                    ascii_start_win_x + ascii_col_width * self.cols.get() as f32,
                )
            } else {
                (hex_end_win_x, hex_end_win_x, hex_end_win_x)
            };

        DataLayout {
            bytes_per_row,

            addr_digits,
            total_rows,

            row_spacing,
            row_height_fixed,
            row_height,
            height_fixed,

            glyph_width,
            hex_byte_width,

            col_spacing,
            hex_col_width,
            ascii_col_width,

            hex_start_win_x,
            hex_end_win_x,
            ascii_sep_win_x,
            ascii_start_win_x,
            total_width,
        }
    }

    fn compute_footer_layout(&self, ui: &Ui, data_layout: &DataLayout) -> FooterLayout {
        // SAFETY: This code doesn't modify the style
        let style = unsafe { ui.style() };

        let win_width = ui.content_region_avail()[0];

        let mut data_preview_height = 0.0;

        if self.flags.contains(Flags::SHOW_DATA_PREVIEW) {
            data_preview_height = ui.frame_height() + style.item_spacing[1] * 3.0;
            match self.data_preview_ty.base {
                DataPreviewBaseTy::I8
                | DataPreviewBaseTy::I16
                | DataPreviewBaseTy::I24
                | DataPreviewBaseTy::I32
                | DataPreviewBaseTy::I64
                | DataPreviewBaseTy::I128 => {
                    data_preview_height +=
                        ui.text_line_height() * 3.0 + style.item_spacing[1] * 2.0;
                }
                DataPreviewBaseTy::F32
                | DataPreviewBaseTy::F64
                | DataPreviewBaseTy::C8
                | DataPreviewBaseTy::C16
                | DataPreviewBaseTy::C32 => {
                    data_preview_height += ui.text_line_height();
                }
            }
        }

        let mut controls_x_remaining = win_width;
        let controls_line_height = ui.frame_height();
        let mut controls_height = controls_line_height;

        macro_rules! add_controls_widget {
            ($width: expr$(, wrap $controls_x_remaining: ident)?) => {
                $(
                    if $controls_x_remaining < $width {
                        $controls_x_remaining = win_width;
                        controls_height += controls_line_height + style.item_spacing[1];
                    }
                )*
                #[allow(unused_assignments)]
                {
                    controls_x_remaining -= $width + style.item_spacing[0];
                }
            };
        }

        if self.flags.contains(Flags::SHOW_VIEW_OPTIONS) {
            let width = ui.calc_text_size("Options...")[0] + style.frame_padding[0] * 2.0;
            add_controls_widget!(width);
        }

        let (range_text, range_width) = if self.flags.contains(Flags::SHOW_RANGE) {
            let text = if self.flags.contains(Flags::UPPERCASE_HEX) {
                format!(
                    "Range {:0addr_digits$X}..{:0addr_digits$X}",
                    self.addr_range.start,
                    self.addr_range.end,
                    addr_digits = data_layout.addr_digits.get() as usize
                )
            } else {
                format!(
                    "Range {:0addr_digits$x}..{:0addr_digits$x}",
                    self.addr_range.start,
                    self.addr_range.end,
                    addr_digits = data_layout.addr_digits.get() as usize
                )
            };
            let width = ui.calc_text_size(&text)[0];
            add_controls_widget!(width, wrap controls_x_remaining);
            (text, width)
        } else {
            (String::new(), 0.0)
        };

        let addr_input_width = {
            let width = ui.calc_text_size(format!(
                "{:0addr_digits$X}",
                0,
                addr_digits = data_layout.addr_digits.get() as usize
            ))[0]
                + style.frame_padding[0] * 2.0;
            add_controls_widget!(width, wrap controls_x_remaining);
            width
        };

        let controls_and_separator_height = controls_height + style.item_spacing[1] * 2.0;

        FooterLayout {
            win_width,
            range_text,
            range_width,
            addr_input_width,
            controls_and_separator_height,
            height: data_preview_height + controls_and_separator_height,
        }
    }

    #[inline]
    pub fn window_auto_width(&mut self, ui: &Ui) -> f32 {
        let mut width = data_layout!(self, ui).total_width;
        if self.scrollbar.is_visible() {
            width += style!(ui, scrollbar_size);
        }
        width + style!(ui, window_padding)[0] * 2.0
    }

    #[inline]
    pub fn handle_options_right_click(&self, ui: &Ui) {
        if self.flags.contains(Flags::SHOW_VIEW_OPTIONS)
            && ui.is_window_hovered_with_flags(WindowHoveredFlags::ROOT_AND_CHILD_WINDOWS)
            && ui.is_mouse_clicked(MouseButton::Right)
        {
            ui.open_popup("options");
        }
    }

    #[inline]
    pub fn draw_buffer_read_only(&mut self, ui: &Ui, mode: DisplayMode, buffer: &[u8]) {
        assert!(
            buffer.len() as WAddr == (self.addr_range.end - self.addr_range.start) as WAddr + 1
        );
        let was_read_only = self.flags.contains(Flags::READ_ONLY);
        self.set_read_only(true);
        let base_addr = self.addr_range.start;
        self.draw_callbacks(
            ui,
            mode,
            &mut &*buffer,
            move |buffer, addr| Some(buffer[(addr - base_addr) as usize]),
            move |_, _, _| {},
        );
        self.set_read_only(was_read_only);
    }

    #[inline]
    pub fn draw_buffer(&mut self, ui: &Ui, mode: DisplayMode, buffer: &mut [u8]) {
        assert!(
            buffer.len() as WAddr == (self.addr_range.end - self.addr_range.start) as WAddr + 1
        );
        let base_addr = self.addr_range.start;
        self.draw_callbacks(
            ui,
            mode,
            buffer,
            move |buffer, addr| Some(buffer[(addr - base_addr) as usize]),
            move |buffer, addr, value| buffer[(addr - base_addr) as usize] = value,
        );
    }

    pub fn draw_callbacks<T: ?Sized>(
        &mut self,
        ui: &Ui,
        mode: DisplayMode,
        cb_data: &mut T,
        mut read: impl FnMut(&mut T, Addr) -> Option<u8>,
        _write: impl FnMut(&mut T, Addr, u8),
    ) {
        let (_window_token, _id_token, total_height) = match mode {
            DisplayMode::Window { title } => {
                let win_width = self.window_auto_width(ui);
                let token = if let Some(token) = ui
                    .window(title)
                    .size_constraints([win_width, -1.0], [win_width, -1.0])
                    .begin()
                {
                    token
                } else {
                    return;
                };
                self.handle_options_right_click(ui);
                (Some(token), None, ui.content_region_max()[1])
            }
            DisplayMode::Child { height } => (None, Some(ui.push_id("memory_editor")), height),
        };

        self.highlights.sort_unstable_by_key(|h| h.0.start);

        if let Some(footer_layout) = &self.footer_layout {
            if footer_layout.win_width != ui.content_region_avail()[0] {
                self.footer_layout = None;
            }
        }

        let data_layout = data_layout!(self, ui);
        let footer_layout = footer_layout!(self, ui, data_layout);

        let mut data_layout_was_invalidated = false;
        let mut footer_layout_was_invalidated = false;

        let frame_padding = ui.push_style_var(StyleVar::FramePadding([0.0; 2]));
        let item_spacing = ui.push_style_var(StyleVar::ItemSpacing([0.0; 2]));

        ui.child_window("##data")
            .movable(false)
            .no_nav()
            .scroll_bar(false)
            .scrollable(false)
            .focused(self.selected_addr.requires_focus)
            .size([0.0, total_height - footer_layout.height])
            .build(|| {
                self.selected_addr.requires_focus = false;

                let win_height = ui.window_size()[1];
                let win_height_fixed = YPos::from(win_height);

                self.scrollbar.window_height = win_height_fixed;
                self.scrollbar.content_height = data_layout.height_fixed;
                self.scrollbar.handle_scroll(ui);

                // Handle keyboard navigation
                {
                    let mut new_addr = None;
                    if ui.is_window_focused() {
                        if ui.is_key_pressed(Key::UpArrow) {
                            new_addr = Some(
                                self.selected_addr
                                    .addr
                                    .saturating_sub(data_layout.bytes_per_row),
                            );
                        }
                        if ui.is_key_pressed(Key::DownArrow) {
                            new_addr = Some(
                                self.selected_addr
                                    .addr
                                    .saturating_add(data_layout.bytes_per_row),
                            );
                        }
                        let col_size = self.col_size.get() as Addr;
                        if ui.is_key_pressed(Key::LeftArrow) {
                            new_addr = Some(if self.flags.contains(Flags::LITTLE_ENDIAN_COLS) {
                                if self.selected_addr.addr % col_size == col_size - 1 {
                                    self.selected_addr.addr.saturating_sub((col_size << 1) - 1)
                                } else {
                                    self.selected_addr.addr.saturating_add(1)
                                }
                            } else {
                                self.selected_addr.addr.saturating_sub(1)
                            });
                        }
                        if ui.is_key_pressed(Key::RightArrow) {
                            new_addr = Some(if self.flags.contains(Flags::LITTLE_ENDIAN_COLS) {
                                if self.selected_addr.addr % col_size == 0 {
                                    self.selected_addr.addr.saturating_add((col_size << 1) - 1)
                                } else {
                                    self.selected_addr.addr.saturating_sub(1)
                                }
                            } else {
                                self.selected_addr.addr.saturating_add(1)
                            });
                        }
                    }
                    if let Some(new_addr) = new_addr {
                        self.selected_addr.set(new_addr, false, &self.addr_range);
                    }
                }

                let win_pos = ui.window_pos();
                let mouse_pos = ui.io().mouse_pos;

                let hex_start_screen_x = win_pos[0] + data_layout.hex_start_win_x;
                let ascii_start_screen_x = win_pos[0] + data_layout.ascii_start_win_x;

                // Handle clicks
                if ui.is_window_hovered() && ui.is_mouse_clicked(MouseButton::Left) {
                    let hex_end_screen_x = win_pos[0] + data_layout.hex_end_win_x;
                    let ascii_end_screen_x = win_pos[0] + data_layout.total_width;

                    let row_base = (YPos::from(mouse_pos[1] - win_pos[1]) + self.scrollbar.scroll)
                        .div_into_int(data_layout.row_height_fixed)
                        as WAddr
                        * data_layout.bytes_per_row as WAddr;

                    let col_and_byte = if (hex_start_screen_x..hex_end_screen_x)
                        .contains(&mouse_pos[0])
                    {
                        let rel_x = mouse_pos[0] - hex_start_screen_x;
                        let col = (rel_x / data_layout.hex_col_width) as WAddr;
                        Some((
                            col,
                            ((rel_x
                                - (col as f32 * data_layout.hex_col_width
                                    + 0.5 * data_layout.col_spacing))
                                / data_layout.hex_byte_width)
                                .clamp(0.0, (self.col_size.get() - 1) as f32)
                                as WAddr,
                        ))
                    } else if (ascii_start_screen_x..ascii_end_screen_x).contains(&mouse_pos[0]) {
                        let rel_x = mouse_pos[0] - ascii_start_screen_x;
                        let col = (rel_x / data_layout.ascii_col_width) as WAddr;
                        Some((
                            col,
                            ((rel_x
                                - (col as f32 * data_layout.ascii_col_width
                                    + 0.5 * data_layout.col_spacing))
                                / data_layout.glyph_width)
                                .clamp(0.0, (self.col_size.get() - 1) as f32)
                                as WAddr,
                        ))
                    } else {
                        None
                    };

                    if let Some((col, mut col_byte)) = col_and_byte {
                        if self.flags.contains(Flags::LITTLE_ENDIAN_COLS) {
                            col_byte = self.col_size.get() as WAddr - 1 - col_byte;
                        }
                        self.selected_addr.set(
                            (row_base + col * self.col_size.get() as WAddr + col_byte)
                                .min(self.addr_range.end as WAddr)
                                as Addr,
                            false,
                            &self.addr_range,
                        );
                    }
                }

                // Scroll to the selected address if it changed
                if self.selected_addr.was_changed {
                    let selected_row = self.selected_addr.addr / data_layout.bytes_per_row;
                    let selection_start_scroll =
                        data_layout.row_height_fixed * selected_row as YPosRaw;

                    if self.scrollbar.scroll >= selection_start_scroll {
                        self.scrollbar.scroll = selection_start_scroll;
                    } else {
                        let selection_end_scroll_minus_content_height = (selection_start_scroll
                            + data_layout.row_height_fixed)
                            .saturating_sub(win_height_fixed);
                        if self.scrollbar.scroll <= selection_end_scroll_minus_content_height {
                            self.scrollbar.scroll = selection_end_scroll_minus_content_height;
                        }
                    }
                }

                // Process scrollbar input first
                if self.scrollbar.is_visible() {
                    self.scrollbar.process(ui);
                }

                // Draw hex/ASCII separator
                if self.flags.contains(Flags::SHOW_ASCII) {
                    let sep_screen_x = win_pos[0] + data_layout.ascii_sep_win_x;
                    let sep_start_y = win_pos[1];
                    ui.get_window_draw_list()
                        .add_line(
                            [sep_screen_x, sep_start_y],
                            [sep_screen_x, sep_start_y + win_height],
                            ui.style_color(StyleColor::Border),
                        )
                        .build();
                }

                // Draw data

                let content_start_y =
                    win_pos[1] - f32::from(self.scrollbar.scroll % data_layout.row_height_fixed);

                self.visible_data_rows = (
                    self.scrollbar
                        .scroll
                        .div_into_int(data_layout.row_height_fixed) as Addr,
                    (self.scrollbar.scroll + win_height_fixed)
                        .div_ceil_into_int(data_layout.row_height_fixed)
                        .min((data_layout.total_rows - 1) as YPosRaw) as Addr,
                )
                    .into();

                let mut cur_addr = self.addr_range.start
                    + self.visible_data_rows.start * data_layout.bytes_per_row;

                let mut cur_highlight_color = None;
                let mut cur_highlight_i = self
                    .highlights
                    .iter()
                    .position(|h| cur_addr <= h.0.end)
                    .unwrap_or(self.highlights.len());

                for rel_row in 0..=self.visible_data_rows.end - self.visible_data_rows.start {
                    let row_start_screen_y =
                        content_start_y + rel_row as f32 * data_layout.row_height;

                    ui.set_cursor_screen_pos([win_pos[0], row_start_screen_y]);
                    ui.text(if self.flags.contains(Flags::UPPERCASE_HEX) {
                        str_buf!(
                            self.str_buffer,
                            "{:0addr_digits$X}:",
                            cur_addr,
                            addr_digits = data_layout.addr_digits.get() as usize
                        )
                    } else {
                        str_buf!(
                            self.str_buffer,
                            "{:0addr_digits$x}:",
                            cur_addr,
                            addr_digits = data_layout.addr_digits.get() as usize
                        )
                    });

                    'cols: for col_i in 0..self.cols.get() {
                        let half_col_spacing = data_layout.col_spacing * 0.5;
                        let hex_col_start_screen_x = hex_start_screen_x
                            + col_i as f32 * data_layout.hex_col_width
                            + half_col_spacing;
                        let ascii_col_start_screen_x = ascii_start_screen_x
                            + col_i as f32 * data_layout.ascii_col_width
                            + half_col_spacing;

                        for byte_i in 0..self.col_size.get() {
                            let byte_x_i = if self.flags.contains(Flags::LITTLE_ENDIAN_COLS) {
                                self.col_size.get() - 1 - byte_i
                            } else {
                                byte_i
                            };

                            let hex_byte_start_screen_x = hex_col_start_screen_x
                                + byte_x_i as f32 * data_layout.hex_byte_width;
                            let ascii_byte_start_screen_x = ascii_col_start_screen_x
                                + byte_x_i as f32 * data_layout.glyph_width;

                            // Calculate the background color, for either the selection or a
                            // highlighted byte
                            let bg_color = if self.selected_addr.addr == cur_addr {
                                Some(ui.style_color(StyleColor::TextSelectedBg).into())
                            } else {
                                if cur_highlight_color.is_some()
                                    && cur_addr > self.highlights[cur_highlight_i].0.end
                                {
                                    cur_highlight_color = None;
                                }
                                if cur_highlight_color.is_none() {
                                    while cur_highlight_i < self.highlights.len() {
                                        let highlight = &self.highlights[cur_highlight_i];
                                        if cur_addr <= highlight.0.end {
                                            if cur_addr >= highlight.0.start {
                                                cur_highlight_color = Some(highlight.1);
                                            }
                                            break;
                                        }
                                        cur_highlight_i += 1;
                                    }
                                }
                                cur_highlight_color
                            };

                            // Draw the background
                            if let Some(color) = bg_color {
                                let draw_list = ui.get_window_draw_list();

                                let spacing_left =
                                    if byte_x_i == 0 { half_col_spacing } else { 0.0 };
                                let spacing_right = if byte_x_i == self.col_size.get() - 1 {
                                    half_col_spacing
                                } else {
                                    0.0
                                };

                                draw_list
                                    .add_rect(
                                        [
                                            hex_byte_start_screen_x - spacing_left,
                                            row_start_screen_y,
                                        ],
                                        [
                                            hex_byte_start_screen_x
                                                + data_layout.hex_byte_width
                                                + spacing_right,
                                            row_start_screen_y + data_layout.row_height,
                                        ],
                                        color,
                                    )
                                    .filled(true)
                                    .build();

                                if self.flags.contains(Flags::SHOW_ASCII) {
                                    draw_list
                                        .add_rect(
                                            [
                                                ascii_byte_start_screen_x - spacing_left,
                                                row_start_screen_y,
                                            ],
                                            [
                                                ascii_byte_start_screen_x
                                                    + data_layout.glyph_width
                                                    + spacing_right,
                                                row_start_screen_y + data_layout.row_height,
                                            ],
                                            color,
                                        )
                                        .filled(true)
                                        .build();
                                }
                            }

                            // Draw the hex/HexII and ASCII
                            if let Some(data) = read(cb_data, cur_addr) {
                                let text_color = ui.style_color(
                                    if self.flags.contains(Flags::GRAY_OUT_ZEROS) && data == 0 {
                                        StyleColor::TextDisabled
                                    } else {
                                        StyleColor::Text
                                    },
                                );

                                ui.set_cursor_screen_pos([
                                    hex_byte_start_screen_x,
                                    row_start_screen_y + 0.5 * data_layout.row_spacing,
                                ]);
                                ui.text_colored(
                                    text_color,
                                    if self.flags.contains(Flags::SHOW_HEXII) {
                                        if (0x20..0x7F).contains(&data) {
                                            str_buf!(self.str_buffer, ".{}", data as char)
                                        } else if data == 0 {
                                            str_buf!(self.str_buffer, "  ")
                                        } else if data == 0xFF {
                                            str_buf!(self.str_buffer, "##")
                                        } else if self.flags.contains(Flags::UPPERCASE_HEX) {
                                            str_buf!(self.str_buffer, "{:02X}", data)
                                        } else {
                                            str_buf!(self.str_buffer, "{:02x}", data)
                                        }
                                    } else if self.flags.contains(Flags::UPPERCASE_HEX) {
                                        str_buf!(self.str_buffer, "{:02X}", data)
                                    } else {
                                        str_buf!(self.str_buffer, "{:02x}", data)
                                    },
                                );

                                if self.flags.contains(Flags::SHOW_ASCII) {
                                    ui.set_cursor_screen_pos([
                                        ascii_byte_start_screen_x,
                                        row_start_screen_y,
                                    ]);
                                    ui.text_colored(
                                        text_color,
                                        str_buf!(
                                            self.str_buffer,
                                            "{}",
                                            if (0x20..0x7F).contains(&data) {
                                                data as char
                                            } else {
                                                '.'
                                            }
                                        ),
                                    );
                                }
                            }

                            if cur_addr >= self.addr_range.end {
                                break 'cols;
                            }
                            cur_addr += 1;
                        }
                    }
                }

                // Draw the scrollbar last on top of everything else
                if self.scrollbar.is_visible() {
                    self.scrollbar.draw(ui);
                }
            });

        drop((item_spacing, frame_padding));

        ui.spacing();
        ui.separator();

        // Data preview

        if self.flags.contains(Flags::SHOW_DATA_PREVIEW) {
            ui.child_window("##data_preview")
                .size([0.0, -footer_layout.controls_and_separator_height])
                .build(|| {
                    static DATA_PREVIEW_BASE_TYS: [DataPreviewBaseTy; 11] = [
                        DataPreviewBaseTy::I8,
                        DataPreviewBaseTy::I16,
                        DataPreviewBaseTy::I24,
                        DataPreviewBaseTy::I32,
                        DataPreviewBaseTy::I64,
                        DataPreviewBaseTy::I128,
                        DataPreviewBaseTy::F32,
                        DataPreviewBaseTy::F64,
                        DataPreviewBaseTy::C8,
                        DataPreviewBaseTy::C16,
                        DataPreviewBaseTy::C32,
                    ];

                    ui.align_text_to_frame_padding();
                    ui.text("Preview as:");

                    ui.same_line();
                    let mut ty_index = self.data_preview_ty.base as usize;
                    ui.set_next_item_width(
                        ui.calc_text_size("0")[0] * 10.0 + style!(ui, frame_padding)[0] * 2.0,
                    );
                    if ui.combo_simple_string(
                        "##data_preview_base_ty",
                        &mut ty_index,
                        &DATA_PREVIEW_BASE_TYS,
                    ) {
                        footer_layout_was_invalidated = true;
                        self.data_preview_ty.base = DATA_PREVIEW_BASE_TYS[ty_index];
                    }

                    if self.data_preview_ty.base.allows_signed() {
                        ui.same_line();
                        ui.checkbox("Signed", &mut self.data_preview_ty.is_signed);
                    }

                    if self.data_preview_ty.base.allows_endianness() {
                        ui.same_line();
                        ui.checkbox("Little endian", &mut self.data_preview_ty.is_little_endian);
                    }

                    macro_rules! read_bytes {
                        ($bytes: expr) => {{
                            let mut value = [0; $bytes];
                            let mut is_valid = true;
                            let mut addr = self.selected_addr.addr;
                            if self.data_preview_ty.is_little_endian {
                                for i in 0..$bytes {
                                    if let Some(byte) = read(cb_data, addr) {
                                        value[$bytes - 1 - i] = byte;
                                    } else {
                                        is_valid = false;
                                        break;
                                    }
                                    if addr >= self.addr_range.end {
                                        is_valid = i >= $bytes - 1;
                                        break;
                                    }
                                    addr += 1;
                                }
                            } else {
                                for i in 0..$bytes {
                                    if let Some(byte) = read(cb_data, addr) {
                                        value[i] = byte;
                                    } else {
                                        is_valid = false;
                                        break;
                                    }
                                    if addr >= self.addr_range.end {
                                        is_valid = i >= $bytes - 1;
                                        break;
                                    }
                                    addr += 1;
                                }
                            }
                            (value, is_valid)
                        }};
                    }

                    macro_rules! preview_int {
                        ($bytes: literal) => {{
                            let (bytes, is_valid) = read_bytes!($bytes);
                            let mut value = 0_u128;
                            for i in 0..$bytes {
                                value |= (bytes[i] as u128) << (($bytes - 1 - i) << 3);
                            }

                            ui.text("Dec:");
                            ui.same_line();
                            ui.text(if is_valid {
                                if self.data_preview_ty.is_signed {
                                    str_buf!(
                                        self.str_buffer,
                                        "{}",
                                        (value as i128) << (128 - ($bytes << 3))
                                            >> (128 - ($bytes << 3))
                                    )
                                } else {
                                    str_buf!(self.str_buffer, "{}", value)
                                }
                            } else {
                                "N/A"
                            });

                            ui.text("Hex:");
                            ui.same_line();
                            ui.text(if is_valid {
                                self.str_buffer.clear();
                                for i in 0..$bytes {
                                    if i != 0 && ($bytes - 1 - i) & 1 == 1 {
                                        self.str_buffer.push(' ');
                                    }
                                    let _ = write!(self.str_buffer, "{:02X}", bytes[i]);
                                }
                                &self.str_buffer
                            } else {
                                "N/A"
                            });

                            ui.text("Bin:");
                            ui.same_line();
                            ui.text(if is_valid {
                                self.str_buffer.clear();
                                for i in 0..$bytes {
                                    if i != 0 {
                                        self.str_buffer.push(' ');
                                    }
                                    let _ = write!(self.str_buffer, "{:08b}", bytes[i]);
                                }
                                &self.str_buffer
                            } else {
                                "N/A"
                            });
                        }};
                    }

                    macro_rules! preview_float {
                        ($ty: ty) => {{
                            let (bytes, is_valid) = read_bytes!(size_of::<$ty>());

                            ui.text("Value:");
                            ui.same_line();
                            ui.text(if is_valid {
                                str_buf!(self.str_buffer, "{}", <$ty>::from_be_bytes(bytes))
                            } else {
                                "N/A"
                            });
                        }};
                    }

                    macro_rules! preview_char {
                        (
                            $bytes: expr,
                            |$bytes_ident: ident| $value: expr, |$value_ident: ident| $char: expr
                        ) => {{
                            let ($bytes_ident, is_valid) = read_bytes!($bytes);

                            ui.text("Value:");
                            ui.same_line();
                            ui.text(if is_valid {
                                let $value_ident = $value;
                                if let Some(char) = $char {
                                    str_buf!(self.str_buffer, "\"{}\"", char.escape_debug())
                                } else {
                                    str_buf!(self.str_buffer, "\\u{{{:x}}}", $value_ident)
                                }
                            } else {
                                "N/A"
                            });
                        }};
                    }

                    match self.data_preview_ty.base {
                        DataPreviewBaseTy::I8 => preview_int!(1),
                        DataPreviewBaseTy::I16 => preview_int!(2),
                        DataPreviewBaseTy::I24 => preview_int!(3),
                        DataPreviewBaseTy::I32 => preview_int!(4),
                        DataPreviewBaseTy::I64 => preview_int!(8),
                        DataPreviewBaseTy::I128 => preview_int!(16),
                        DataPreviewBaseTy::F32 => preview_float!(f32),
                        DataPreviewBaseTy::F64 => preview_float!(f64),
                        DataPreviewBaseTy::C8 => {
                            preview_char!(1, |bytes| bytes[0], |value| Some(char::from(value)));
                        }
                        DataPreviewBaseTy::C16 => {
                            preview_char!(2, |bytes| u16::from_be_bytes(bytes), |value| {
                                char::decode_utf16(once(value)).next().unwrap().ok()
                            });
                        }
                        DataPreviewBaseTy::C32 => {
                            preview_char!(4, |bytes| u32::from_be_bytes(bytes), |value| {
                                char::from_u32(u32::from_be_bytes(bytes))
                            });
                        }
                    }
                });

            ui.separator();
        }

        // Options

        if self.flags.contains(Flags::SHOW_VIEW_OPTIONS) {
            if ui.button("Options...") {
                ui.open_popup("options");
            }

            ui.popup("options", || {
                let mut cols = self.cols.get();
                if Drag::new("##cols")
                    .display_format("Cols: %d")
                    .build(ui, &mut cols)
                {
                    data_layout_was_invalidated = true;
                    self.cols = NonZeroU8::new(cols.max(1)).unwrap();
                }

                ui.same_line();

                let mut col_size = self.col_size.get();
                if Drag::new("##col_size")
                    .display_format("Col size: %d")
                    .build(ui, &mut col_size)
                {
                    data_layout_was_invalidated = true;
                    self.col_size = NonZeroU8::new(col_size.max(1)).unwrap();
                }

                ui.checkbox_flags("Gray out zeros", &mut self.flags, Flags::GRAY_OUT_ZEROS);

                let uppercase_hex_changed =
                    ui.checkbox_flags("Uppercase hex", &mut self.flags, Flags::UPPERCASE_HEX);
                footer_layout_was_invalidated |= uppercase_hex_changed;
                // NB: Kind of a hack, used to update the address string
                self.selected_addr.was_changed |= uppercase_hex_changed;

                ui.checkbox_flags(
                    "Little endian cols",
                    &mut self.flags,
                    Flags::LITTLE_ENDIAN_COLS,
                );

                ui.checkbox_flags("Show HexII", &mut self.flags, Flags::SHOW_HEXII);

                data_layout_was_invalidated |=
                    ui.checkbox_flags("Show ASCII", &mut self.flags, Flags::SHOW_ASCII);

                footer_layout_was_invalidated |= ui.checkbox_flags(
                    "Show data preview",
                    &mut self.flags,
                    Flags::SHOW_DATA_PREVIEW,
                );
            });
            
            ui.same_line();
        }

        // Address range bounds

        let data_layout = data_layout!(self, ui);
        if self.flags.contains(Flags::SHOW_RANGE) {
            if ui.content_region_avail()[0] < footer_layout.range_width {
                ui.new_line();
            }
            ui.align_text_to_frame_padding();
            ui.text(&footer_layout.range_text);
            ui.same_line();
        }

        // Address input field

        if ui.content_region_avail()[0] < footer_layout.addr_input_width {
            ui.new_line();
        }
        ui.set_next_item_width(footer_layout.addr_input_width);

        if self.selected_addr.was_changed {
            self.selected_addr.input_buffer.clear();
            if self.flags.contains(Flags::UPPERCASE_HEX) {
                let _ = write!(
                    self.selected_addr.input_buffer,
                    "{:0addr_digits$X}",
                    self.selected_addr.addr,
                    addr_digits = data_layout.addr_digits.get() as usize
                );
            } else {
                let _ = write!(
                    self.selected_addr.input_buffer,
                    "{:0addr_digits$x}",
                    self.selected_addr.addr,
                    addr_digits = data_layout.addr_digits.get() as usize
                );
            }
            self.selected_addr.was_changed = false;
        }

        if ui
            .input_text("##address", &mut self.selected_addr.input_buffer)
            .auto_select_all(true)
            .chars_hexadecimal(true)
            .enter_returns_true(true)
            .no_horizontal_scroll(true)
            .build()
        {
            if let Ok(addr) = Addr::from_str_radix(&self.selected_addr.input_buffer, 16) {
                self.selected_addr.set(addr, true, &self.addr_range);
            }
        };

        // Finalize

        if data_layout_was_invalidated {
            self.data_layout = None;
        }

        if footer_layout_was_invalidated {
            self.footer_layout = None;
        }
    }
}
