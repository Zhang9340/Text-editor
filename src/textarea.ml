open Graphics

type t = {
  mutable x : int;
  mutable y : int;
  mutable width : int;
  mutable height : int;
  mutable bg_color : Graphics.color;
  mutable text_color : Graphics.color;
  mutable cursor_color : Graphics.color;
  mutable text : string;
  mutable cursor_pos : int;
}

let create x y width height bg_color text_color cursor_color =
  {
    x;
    y;
    width;
    height;
    bg_color;
    text_color;
    cursor_color;
    text = "";
    cursor_pos = 0;
  }

let contains_point area x y =
  x >= area.x
  && x < area.x + area.width
  && y >= area.y
  && y < area.y + area.height

let draw area =
  Graphics.set_color area.bg_color;
  Graphics.fill_rect area.x area.y area.width area.height;
  Graphics.set_color area.text_color;
  Graphics.moveto (area.x + 10) (area.y + 10);
  Graphics.draw_string area.text;
  Graphics.set_color area.cursor_color;
  Graphics.moveto
    (area.x + 10 + Graphics.text_width (String.sub area.text 0 area.cursor_pos))
    (area.y + 10);
  Graphics.lineto
    (area.x + 10 + Graphics.text_width (String.sub area.text 0 area.cursor_pos))
    (area.y + 10 + Graphics.text_height "A")

let get_text area = area.text

let save_to_file area filename =
  let out_channel = open_out filename in
  output_string out_channel area.text;
  close_out out_channel;
  true

let handle_event area event =
  match event with
  | Graphics.Key_pressed key -> begin
      match key with
      | '\b' ->
          (* backspace *)
          if area.cursor_pos > 0 then begin
            area.text <-
              String.sub area.text 0 (area.cursor_pos - 1)
              ^ String.sub area.text area.cursor_pos
                  (String.length area.text - area.cursor_pos);
            area.cursor_pos <- area.cursor_pos - 1
          end
      | '\r' ->
          (* return *)
          area.text <-
            String.sub area.text 0 area.cursor_pos
            ^ "\n"
            ^ String.sub area.text area.cursor_pos
                (String.length area.text - area.cursor_pos);
          area.cursor_pos <- area.cursor_pos + 1
      | '\027' ->
          (* escape *)
          ()
      | _ ->
          area.text <-
            String.sub area.text 0 area.cursor_pos
            ^ String.make 1 key
            ^ String.sub area.text area.cursor_pos
                (String.length area.text - area.cursor_pos);
          area.cursor_pos <- area.cursor_pos + 1
    end
  | _ -> ()
