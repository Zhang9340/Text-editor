open Notty

(* open Notty_unix *)
open Editor
open Text_editor

let select_flag = ref false

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
  | MousePressLeft of int * int

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

let get_select_start s =
  let oe1 = s.selection_start in
  let oe2 = s.selection_end in
  let e1 = Option.get oe1 in
  let e2 = Option.get oe2 in
  let e1_t1 = tuple_get_first e1 in
  let e1_t2 = tuple_get_second e1 in
  let e2_t1 = tuple_get_first e2 in
  let e2_t2 = tuple_get_second e2 in
  if e1_t1 < e2_t1 then e1
  else if e1_t1 > e2_t1 then e2
  else if e1_t2 < e2_t2 then e1
  else e2

let get_select_end s =
  let oe1 = s.selection_start in
  let oe2 = s.selection_end in
  let e1 = Option.get oe1 in
  let e2 = Option.get oe2 in
  let e1_t1 = tuple_get_first e1 in
  let e1_t2 = tuple_get_second e1 in
  let e2_t1 = tuple_get_first e2 in
  let e2_t2 = tuple_get_second e2 in
  if e1_t1 > e2_t1 then e1
  else if e1_t1 < e2_t1 then e2
  else if e1_t2 > e2_t2 then e1
  else e2

let is_select s nrow i =
  if !select_flag = true then
    if tuple_get_first (get_select_start s) = tuple_get_first (get_select_end s)
    then
      if
        tuple_get_second (get_select_start s) <= i
        && tuple_get_second (get_select_end s) >= i
        && tuple_get_first (get_select_end s) = nrow
      then true
      else false
    else if
      tuple_get_first (get_select_start s) < nrow
      && tuple_get_first (get_select_end s) > nrow
    then true
    else if
      tuple_get_first (get_select_start s) = nrow
      && tuple_get_second (get_select_start s) <= i
    then true
    else if
      tuple_get_first (get_select_end s) = nrow
      && tuple_get_second (get_select_end s) >= i
    then true
    else false
  else false

let convert_state (s : editor_state) =
  let string_to_image str flag nrow =
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
          else if
            (*if select flag = true, and if nrow and i between selection, then
              highlight*)
            is_select s nrow i = true
          then
            let ch = str.[i] in
            let ch_img = I.uchar (A.bg A.red) (Uchar.of_char ch) 1 1 in
            loop (i + 1) I.(img <|> ch_img)
          else
            let ch = str.[i] in
            let ch_img = I.uchar (A.fg A.blue) (Uchar.of_char ch) 1 1 in
            loop (i + 1) I.(img <|> ch_img)
        else img
      in
      loop 0 I.empty
  in
  let header_to_image str =
    let len = String.length str in
    let rec loop i img =
      if i < len then
        let ch = str.[i] in
        let ch_img =
          I.uchar A.(fg lightgreen ++ bg white) (Uchar.of_char ch) 1 1
        in
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
  let end_line =
    "-------------------------------------next \
     page-------------------------------------"
  in
  let end_file =
    "-------------------------------------end of \
     file-------------------------------------"
  in
  let rec convert (c : string list) (inc : int) acc =
    match c with
    | [] ->
        I.(acc <-> string_to_image " " false inc <-> header_to_image end_file)
    | h :: t ->
        let left = tuple_get_first display_range in
        let right = tuple_get_second display_range in

        if inc < left then convert t (inc + 1) acc
        else if inc > right then
          I.(acc <-> string_to_image " " false inc <-> header_to_image end_line)
        else if inc = cursor_row then
          convert t (inc + 1) I.(acc <-> string_to_image h true inc)
        else convert t (inc + 1) I.(acc <-> string_to_image h false inc)
  in

  let header =
    "[<-] LEFT, [^] UP, [->] RIGHT, [v] DOWN, [Enter] SAVE, [Esc] QUIT"
  in
  convert s.text 0 I.(header_to_image header <-> string_to_image " " false ~-1)

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
      if !select_flag then s else move_cursor s (0, -1)
  | RIGHT ->
      helper (move_cursor s (0, 1));
      if !select_flag then s else move_cursor s (0, 1)
  | UP ->
      helper (move_cursor s (-1, 0));
      if !select_flag then s else move_cursor s (-1, 0)
  | DOWN ->
      helper (move_cursor s (1, 0));
      if !select_flag then s else move_cursor s (1, 0)
  | MousePressLeft (x, y) ->
      let rows = List.length s.text - 1 in
      let row_pos = if y > rows then rows else y in
      let cols_in_row = String.length (get_nth_elm s.text row_pos) - 1 in
      let col_pos = if x > cols_in_row then cols_in_row else x in
      let newstate =
        if !select_flag = false then
          {
            text = s.text;
            cursor_pos = s.cursor_pos;
            selection_start = Some s.cursor_pos;
            selection_end = Some (row_pos - 2, col_pos);
          }
        else
          {
            text = s.text;
            cursor_pos = (row_pos - 2, col_pos);
            selection_start = Some (row_pos - 2, col_pos);
            selection_end = Some (row_pos - 2, col_pos);
          }
      in
      select_flag := not !select_flag;
      helper newstate;
      newstate
  (* helper (mover_) *)
  | _ -> s

let insert_char_cmd (s : editor_state) (c : char) =
  (* (insert_str s (tuple_get_first s.cursor_pos) (tuple_get_second
     s.cursor_pos) (String.make 1 c)) *)
  move_cursor_cmd RIGHT
    (insert_str (is_last_insert_space s)
       (tuple_get_first s.cursor_pos)
       (tuple_get_second s.cursor_pos)
       (String.make 1 c))

let delete_char_cmd (s : editor_state) =
  if !select_flag = true then (
    assert !select_flag;
    select_flag := false;
    delete_selection
      {
        text = s.text;
        cursor_pos = s.cursor_pos;
        selection_start = Some (get_select_start s);
        selection_end =
          Some
            ( tuple_get_first (get_select_end s),
              tuple_get_second (get_select_end s) + 1 );
      })
  else (
    select_flag := false;
    delete s)

let save_file_cmd (s : editor_state) (name : string) =
  save_file s name;
  s
