(* open Graphics *)

type editor_state = {
  text : string list;
  cursor_pos : int;
  selection_start : int option;
  selection_end : int option;
}

let create_editor_state () =
  { text = []; cursor_pos = 0; selection_start = None; selection_end = None }

let load_file filename =
  let chan = open_in filename in
  let rec loop acc =
    try
      let line = input_line chan in
      loop (line :: acc)
    with End_of_file ->(
      close_in chan;
      {
        text = List.rev acc;
        cursor_pos = 0;
        selection_start = None;
        selection_end = None;
      })
  in
  loop []

(* let text_format state =
  let lst = state.text in
  let rec loop ls = match ls with
    | hd :: tl -> 
      (if tl == [] then hd else hd ^ "\n" ^ (loop tl))
    | [] -> ""
  in loop lst *)


let save_file state filename =
  let chan = open_out filename in
  List.iter (fun line -> output_string chan (line ^ "\n")) state.text;
  close_out chan

let rec sublist b e l =
  match l with
  | [] -> failwith "sublist"
  | h :: t ->
      let tail = if e = 0 then [] else sublist (b - 1) (e - 1) t in
      if b > 0 then tail else h :: tail

let insert_char state c =
  let text =
    match (state.selection_start, state.selection_end) with
    | Some start_pos, Some end_pos ->
        let prefix = sublist 0 start_pos state.text in
        let suffix =
          sublist end_pos (List.length state.text - end_pos) state.text
        in
        prefix @ [ String.make 1 c ] @ suffix
    | _ ->
        let prefix = sublist 0 state.cursor_pos state.text in
        let suffix =
          sublist state.cursor_pos
            (List.length state.text - state.cursor_pos)
            state.text
        in
        prefix @ [ String.make 1 c ] @ suffix
  in
  { state with text; cursor_pos = state.cursor_pos + 1 }

let delete_char state =
  let text =
    match (state.selection_start, state.selection_end) with
    | Some start_pos, Some end_pos ->
        let prefix = sublist 0 start_pos state.text in
        let suffix =
          sublist end_pos (List.length state.text - end_pos) state.text
        in
        prefix @ suffix
    | _ ->
        let prefix = sublist 0 (state.cursor_pos - 1) state.text in
        let suffix =
          sublist state.cursor_pos
            (List.length state.text - state.cursor_pos)
            state.text
        in
        prefix @ suffix
  in
  { state with text; cursor_pos = state.cursor_pos - 1 }

let move_cursor state offset =
  let new_pos =
    max 0 (min (List.length state.text) (state.cursor_pos + offset))
  in
  {
    state with
    cursor_pos = new_pos;
    selection_start = None;
    selection_end = None;
  }
(*
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

*)

let select_text state start_pos end_pos =
  { state with selection_start = Some start_pos; selection_end = Some end_pos }



(*
let window_title = "Text Editor"
let window_width = 800
let window_height = 600
let text_width = 600
let text_height = 400
let button_width = 100
let button_height = 30

let main () =
  (* Open a window *)
  open_graph (Printf.sprintf " %dx%d" window_width window_height);
  set_window_title window_title;

  (* Create a text area *)
  let text_area =
    let x = (window_width - text_width) / 2 in
    let y = (window_height - text_height) / 2 in
    let width = text_width in
    let height = text_height in
    let bg_color = white in
    let text_color = black in
    let cursor_color = black in
    Textarea.create x y width height bg_color text_color cursor_color
  in
  Textarea.draw text_area;

  (* Create a "Save" button *)
  let save_button =
    let x = (window_width - button_width) / 2 in
    let y = ((window_height - button_height) / 2) + (text_height / 2) + 20 in
    let width = button_width in
    let height = button_height in
    let text = "Save" in
    let bg_color = blue in
    let text_color = white in
    Button.create x y width height text bg_color text_color
  in
  Button.draw save_button;

  (* Event loop *)
  let rec event_loop () =
    let event = wait_next_event [ Button_down ] in
    if Button.contains_point save_button event.mouse_x event.mouse_y then begin
      let text = Textarea.get_text text_area in
      let _ = Textarea.save_to_file text_area "output.txt" in
      close_graph ();
      exit 0
    end
    else if Textarea.contains_point text_area event.mouse_x event.mouse_y then begin
      let _ = Textarea.handle_event text_area event in
      Textarea.draw text_area;
      event_loop ()
    end
    else event_loop ()
  in
  event_loop ()
  *)
