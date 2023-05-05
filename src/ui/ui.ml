open Notty

(* open Notty_unix *)
open Editor
open Text_editor

(* Direction command. Keyboard action. *)
type cmd =
  | UP
  | LEFT
  | RIGHT
  | DOWN
  | INSERT of char
  | DELETE
  | REPLACE
  | INIT
  | SAVE of string

(* let string_to_image s = let len = String.length s in let rec loop i img = if
   i < len then let ch = s.[i] in let ch_img = I.uchar (A.fg A.blue)
   (Uchar.of_char ch) 1 1 in loop (i + 1) I.(img <|> ch_img) else img in loop 0
   I.empty

   let rec convert (c : string list) = (* helper function *) match c with | []
   -> I.empty | h :: t -> I.(string_to_image h <-> convert t)

   let convert_state (s : editor_state) = convert s.text *)

let tuple_get_first t =
  match t with
  | r, _ -> r

let tuple_get_second t =
  match t with
  | _, c -> c

let convert_state (s : editor_state) =
  let string_to_image str flag =
    if str = "" && flag = true then
      I.uchar (A.bg A.white) (Uchar.of_char ' ') 1 1
    else if str = "" && flag = false then
      I.uchar (A.fg A.blue) (Uchar.of_char ' ') 1 1
    else
      let len = String.length str in
      let rec loop i img =
        if i < len then
          if i = tuple_get_second s.cursor_pos && flag = true then
            let ch = str.[i] in
            let ch_img = I.uchar (A.bg A.white) (Uchar.of_char ch) 1 1 in
            loop (i + 1) I.(img <|> ch_img)
          else
            let ch = str.[i] in
            let ch_img = I.uchar (A.fg A.blue) (Uchar.of_char ch) 1 1 in
            loop (i + 1) I.(img <|> ch_img)
        else img
      in
      loop 0 I.empty
  in

  let cursor_row = tuple_get_first s.cursor_pos in

  let display_range =
    let text_len = List.length s.text in
    let left = cursor_row / 40 * 40 in
    let right = if left + 40 > text_len then text_len else left + 40 in
    let newleft = left - 2 in
    let newright = right + 2 in
    (newleft, newright)
  in

  let rec convert (c : string list) (inc : int) acc =
    match c with
    | [] -> I.empty
    | h :: t ->
        let left = tuple_get_first display_range in
        let right = tuple_get_second display_range in
        if inc < left then convert t (inc + 1) acc
        else if inc > right then acc
        else if inc = cursor_row then
          convert t (inc + 1) I.(acc <-> string_to_image h true)
          (* else I.(string_to_image h false <-> convert t (inc + 1) acc) *)
        else convert t (inc + 1) I.(acc <-> string_to_image h false)
  in

  convert s.text 0 I.empty

let write_int_to_file (filename : string) (tuple : int * int) : unit =
  let oc = open_out filename in
  let x, y = tuple in
  let content = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")\n" in
  output_string oc content;
  close_out oc

let helper s = write_int_to_file "output.txt" s.cursor_pos

(* input editor state and direction return new editor state*)
let move_cursor_cmd (dir : cmd) (s : editor_state) =
  match dir with
  | LEFT ->
      helper (move_cursor s (0, -1));
      move_cursor s (0, -1)
  | RIGHT ->
      helper (move_cursor s (0, 1));
      move_cursor s (0, 1)
  | UP ->
      helper (move_cursor s (-1, 0));
      move_cursor s (-1, 0)
  | DOWN ->
      helper (move_cursor s (1, 0));
      move_cursor s (1, 0)
  | _ -> s

let insert_char_cmd (s : editor_state) (c : char) =
  (* (insert_str s (tuple_get_first s.cursor_pos) (tuple_get_second
     s.cursor_pos) (String.make 1 c)) *)
  move_cursor_cmd RIGHT
    (insert_str (is_last_insert_space s)
       (tuple_get_first s.cursor_pos)
       (tuple_get_second s.cursor_pos)
       (String.make 1 c))

let delete_char_cmd (s : editor_state) = delete s

let save_file_cmd (s : editor_state) (name : string) =
  save_file s name;
  s
