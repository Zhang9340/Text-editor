(* open Graphics *)

type editor_state = {
  text : string list;
  cursor_pos : int * int;
  selection_start : int option * int option;
  selection_end : int option * int option;
}

let create_editor_state () =
  {
    text = [];
    cursor_pos = (0, 0);
    selection_start = (None, None);
    selection_end = (None, None);
  }

let get_text state = state.text

let load_file filename =
  let chan = open_in filename in
  let rec loop acc =
    try
      let line = input_line chan in
      loop (line :: acc)
    with End_of_file ->
      close_in chan;
      {
        text = List.rev acc;
        cursor_pos = (0, 0);
        selection_start = (None, None);
        selection_end = (None, None);
      }
  in
  loop []

let text_format state =
  let lst = get_text state in
  let rec loop ls =
    match ls with
    | hd :: tl -> if tl == [] then hd else hd ^ "\n" ^ loop tl
    | [] -> ""
  in
  loop lst

let save_file state filename =
  let chan = open_out filename in
  List.iter (fun line -> output_string chan (line ^ "\n")) (get_text state);
  close_out chan

let rec sublist b e l =
  if e <= b then []
  else
    match l with
    | [] -> []
    | h :: t ->
        let tail = if e = 0 then [] else sublist (b - 1) (e - 1) t in
        if b > 0 then tail else h :: tail

let update_in_one_row state s start_pos_r start_pos_c end_pos_c =
  let stext = List.nth (get_text state) start_pos_r in
  let prefix = String.sub stext 0 start_pos_c in
  let suffix =
    String.sub stext end_pos_c (String.length stext - end_pos_c - 1)
  in
  prefix ^ s ^ suffix

let update_row state s =
  match (state.selection_start, state.selection_end) with
  | (Some start_pos_r, Some start_pos_c), (Some end_pos_r, Some end_pos_c) ->
      if end_pos_r - start_pos_r = 0 then
        update_in_one_row state s start_pos_r start_pos_c end_pos_c
      else " " (* TODO: haven't implemented switch line change *)
  | _ -> failwith "fail to update row"

let update_all_rows state r =
  match (state.selection_start, state.selection_end) with
  | (Some start_pos_r, _), (Some end_pos_r, _) ->
      let prefix = sublist 0 (start_pos_r - 1) (get_text state) in
      let suffix =
        sublist end_pos_r
          (List.length (get_text state) - end_pos_r)
          (get_text state)
      in
      prefix @ [ r ] @ suffix
  | _ -> failwith "fail to update column"

let option_tuple_get_int t =
  match t with
  | Some a, Some b -> (a, b)
  | _ -> failwith "internal error"

let replace_str state s =
  let text = s |> update_row state |> update_all_rows state in
  { state with text; cursor_pos = option_tuple_get_int state.selection_start }

let select_text state start_pos_r start_pos_c end_pos_r end_pos_c =
  {
    state with
    selection_start = (Some start_pos_r, Some start_pos_c);
    selection_end = (Some end_pos_r, Some end_pos_c);
  }

let tuple_get_first t =
  match t with
  | r, _ -> r

let tuple_get_second t =
  match t with
  | _, c -> c

let move_cursor state (offset_r, offset_c) =
  let new_pos_r =
    max 0
      (min (List.length state.text)
         (tuple_get_first state.cursor_pos + offset_r))
  in
  let new_pos_c =
    max 0
      (min (List.length state.text)
         (tuple_get_second state.cursor_pos + offset_c))
  in
  {
    state with
    cursor_pos = (new_pos_r, new_pos_c);
    selection_start = (None, None);
    selection_end = (None, None);
  }

(* let delete_char state = let text = match (state.selection_start,
   state.selection_end) with | Some start_pos, Some end_pos -> let prefix =
   sublist 0 start_pos state.text in let suffix = sublist end_pos (List.length
   state.text - end_pos) state.text in prefix @ suffix | _ -> let prefix =
   sublist 0 (state.cursor_pos - 1) state.text in let suffix = sublist
   state.cursor_pos (List.length state.text - state.cursor_pos) state.text in
   prefix @ suffix in { state with text; cursor_pos = state.cursor_pos - 1 } *)

(* type t = { mutable x : int; mutable y : int; mutable width : int; mutable
   height : int; mutable bg_color : Graphics.color; mutable text_color :
   Graphics.color; mutable cursor_color : Graphics.color; mutable text : string;
   mutable cursor_pos : int; }

   let create x y width height bg_color text_color cursor_color = { x; y; width;
   height; bg_color; text_color; cursor_color; text = ""; cursor_pos = 0; }

   let contains_point area x y = x >= area.x && x < area.x + area.width && y >=
   area.y && y < area.y + area.height *)

(* let window_title = "Text Editor" let window_width = 800 let window_height =
   600 let text_width = 600 let text_height = 400 let button_width = 100 let
   button_height = 30

   let main () = (* Open a window *) open_graph (Printf.sprintf " %dx%d"
   window_width window_height); set_window_title window_title;

   (* Create a text area *) let text_area = let x = (window_width - text_width)
   / 2 in let y = (window_height - text_height) / 2 in let width = text_width in
   let height = text_height in let bg_color = white in let text_color = black in
   let cursor_color = black in Textarea.create x y width height bg_color
   text_color cursor_color in Textarea.draw text_area;

   (* Create a "Save" button *) let save_button = let x = (window_width -
   button_width) / 2 in let y = ((window_height - button_height) / 2) +
   (text_height / 2) + 20 in let width = button_width in let height =
   button_height in let text = "Save" in let bg_color = blue in let text_color =
   white in Button.create x y width height text bg_color text_color in
   Button.draw save_button;

   (* Event loop *) let rec event_loop () = let event = wait_next_event [
   Button_down ] in if Button.contains_point save_button event.mouse_x
   event.mouse_y then begin let text = Textarea.get_text text_area in let _ =
   Textarea.save_to_file text_area "output.txt" in close_graph (); exit 0 end
   else if Textarea.contains_point text_area event.mouse_x event.mouse_y then
   begin let _ = Textarea.handle_event text_area event in Textarea.draw
   text_area; event_loop () end else event_loop () in event_loop () *)
