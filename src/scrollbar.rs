use super::y_pos::{SignedYPos, YPos};
use imgui::{MouseButton, StyleColor, Ui};

pub struct Scrollbar {
    pub scroll: YPos,
    pub window_height: YPos,
    pub content_height: YPos,
    is_hovered: bool,
    is_grabbing: bool,
    grab_start_y: f32,
    grab_start_scroll: YPos,
}

struct Layout {
    x: f32,
    y: f32,
    width: f32,
    height: f32,
    scroll_max: YPos,
    grab_start: f32,
    grab_height: f32,
}

impl Scrollbar {
    #[inline]
    pub fn new() -> Self {
        Scrollbar {
            scroll: YPos(0),
            window_height: YPos(0),
            content_height: YPos(0),
            is_hovered: false,
            is_grabbing: false,
            grab_start_y: 0.0,
            grab_start_scroll: YPos(0),
        }
    }

    pub fn is_visible(&self) -> bool {
        self.content_height > self.window_height
    }

    fn scroll_max(&self) -> YPos {
        self.content_height.saturating_sub(self.window_height)
    }

    pub fn handle_scroll(&mut self, ui: &Ui) {
        let scroll_max = self.scroll_max();
        if scroll_max.0 != 0 {
            self.scroll = {
                let mut scroll = self.scroll.as_signed();
                if ui.is_window_hovered() {
                    scroll -= SignedYPos::from(
                        ui.io().mouse_wheel * 3.0 * ui.text_line_height_with_spacing(),
                    );
                }
                scroll
                    .clamp(SignedYPos(0), scroll_max.as_signed())
                    .as_unsigned()
            };
        }
    }

    fn compute_layout(&self, ui: &Ui) -> Layout {
        let win_pos = ui.window_pos();
        let win_content_size = ui.content_region_max();
        let width = style!(ui, scrollbar_size);
        let height = win_content_size[1];
        let x = win_pos[0] + win_content_size[0] - width;
        let y = win_pos[1];

        let scroll_max = self.scroll_max();

        let grab_height = (f32::from(self.window_height / self.content_height) * height).max(width);
        let grab_start = y + (height - grab_height).max(0.0) * f32::from(self.scroll / scroll_max);

        Layout {
            x,
            y,
            width,
            height,
            scroll_max,
            grab_start,
            grab_height,
        }
    }

    pub fn process(&mut self, ui: &Ui) {
        let Layout {
            x,
            y,
            width,
            height,
            scroll_max,
            grab_start,
            grab_height,
        } = self.compute_layout(ui);

        ui.set_cursor_screen_pos([x, y]);
        ui.invisible_button("##scrollbar", [width, height]);
        ui.set_item_allow_overlap();
        self.is_hovered = ui.is_item_hovered();

        if ui.is_mouse_down(MouseButton::Left) {
            let mouse_y = ui.io().mouse_pos[1];
            if ui.is_item_clicked() {
                if !(grab_start..grab_start + grab_height).contains(&mouse_y) {
                    let new_scroll_ratio = ((mouse_y - y - grab_height * 0.5)
                        / (height - grab_height))
                        .clamp(0.0, 1.0);
                    self.scroll = scroll_max * new_scroll_ratio;
                }
                self.is_grabbing = true;
                self.grab_start_y = mouse_y;
                self.grab_start_scroll = self.scroll;
            } else if self.is_grabbing {
                let delta = (mouse_y - self.grab_start_y) / (height - grab_height);
                self.scroll = (self.grab_start_scroll.as_signed()
                    + scroll_max.as_signed() * SignedYPos::from(delta))
                .clamp(SignedYPos(0), scroll_max.as_signed())
                .as_unsigned();
            }
        } else {
            self.is_grabbing = false;
        }
    }

    pub fn draw(&mut self, ui: &Ui) {
        let Layout {
            x,
            y,
            width,
            height,
            grab_start,
            grab_height,
            ..
        } = self.compute_layout(ui);

        let scrollbar_color = ui.style_color(StyleColor::ScrollbarBg);
        let grab_color = ui.style_color(if self.is_grabbing {
            StyleColor::ScrollbarGrabActive
        } else if self.is_hovered {
            StyleColor::ScrollbarGrabHovered
        } else {
            StyleColor::ScrollbarGrab
        });

        let draw_list = ui.get_window_draw_list();
        draw_list
            .add_rect([x, y], [x + width, y + height], scrollbar_color)
            .filled(true)
            .build();
        draw_list
            .add_rect(
                [x, grab_start],
                [x + width, grab_start + grab_height],
                grab_color,
            )
            .filled(true)
            .rounding(width * 0.5)
            .build();
    }
}
