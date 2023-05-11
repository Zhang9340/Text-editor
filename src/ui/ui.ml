open Notty
open Editor
open Text_editor

let select_flag = ref false
let clip_board = ref []
let max (a : int) (b : int) = if a > b then a else b

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
  | NEWLINE
  | COPY
  | PASTE

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

let rec loop s str i img flag nrow =
  let len = String.length str in
  if i < len then
    if i = tuple_get_second s.cursor_pos && flag = true then
      let ch = str.[i] in
      let ch_img = I.uchar (A.bg A.white) (Uchar.of_char ch) 1 1 in
      loop s str (i + 1) I.(img <|> ch_img) flag nrow
    else if
      (*if select flag = true, and if nrow and i between selection, then
        highlight*)
      is_select s nrow i = true
    then
      let ch = str.[i] in
      let ch_img = I.uchar (A.bg A.red) (Uchar.of_char ch) 1 1 in
      loop s str (i + 1) I.(img <|> ch_img) flag nrow
    else
      let ch = str.[i] in
      let ch_img = I.uchar (A.fg A.blue) (Uchar.of_char ch) 1 1 in
      loop s str (i + 1) I.(img <|> ch_img) flag nrow
  else img

let string_to_image s str flag nrow =
  if str = "" && flag = true then I.uchar (A.bg A.white) (Uchar.of_char ' ') 1 1
  else if str = "" && flag = false then
    I.uchar (A.fg A.cyan) (Uchar.of_char ' ') 1 1
  else loop s str 0 I.empty flag nrow

let header_to_image str =
  let len = String.length str in
  let rec loop i img =
    if i < len then
      let ch = str.[i] in
      let ch_img = I.uchar A.(fg blue ++ bg white) (Uchar.of_char ch) 1 1 in
      loop (i + 1) I.(img <|> ch_img)
    else img
  in
  loop 0 I.empty

let ind_to_image str =
  let len = String.length str in
  let rec loop i img =
    if i < len then
      let ch = str.[i] in
      let ch_img = I.uchar A.(fg yellow) (Uchar.of_char ch) 1 1 in
      loop (i + 1) I.(img <|> ch_img)
    else img
  in
  loop 0 I.empty

let header =
  "[<-] LEFT, [^] UP, [->] RIGHT, [v] DOWN, [Enter] SAVE, [Esc] QUIT, [Left \
   Click] SELECT , [Control c] Copy , [Control p] Paste"

let select_indicator = "selecting..."
let insert_indicator = "insert"

let end_file =
  "-------------------------------------end of \
   file-------------------------------------"

let end_line =
  "-------------------------------------next \
   page-------------------------------------"

let cursor_row s = tuple_get_first s.cursor_pos

let display_range s =
  let text_len = List.length s.text in
  let left = cursor_row s / 40 * 40 in
  let right = if left + 40 > text_len then text_len else left + 40 in
  let newleft = left - 2 in
  let newright = right + 2 in
  (newleft, newright)

let rec convert (s : editor_state) (c : string list) (inc : int) acc =
  match c with
  | [] ->
      I.(acc <-> string_to_image s " " false inc <-> header_to_image end_file)
  | h :: t ->
      let left = tuple_get_first (display_range s) in
      let right = tuple_get_second (display_range s) in

      if inc < left then convert s t (inc + 1) acc
      else if inc > right then
        I.(acc <-> string_to_image s " " false inc <-> header_to_image end_line)
      else if inc = cursor_row s then
        convert s t (inc + 1) I.(acc <-> string_to_image s h true inc)
      else convert s t (inc + 1) I.(acc <-> string_to_image s h false inc)

let convert_state (s : editor_state) =
  if !select_flag = true then
    convert s s.text 0
      I.(
        header_to_image header
        <-> ind_to_image select_indicator
        <-> string_to_image s " " false ~-1)
  else
    convert s s.text 0
      I.(
        header_to_image header
        <-> ind_to_image insert_indicator
        <-> string_to_image s " " false ~-1)

let get_first_element lst =
  match lst with
  | head :: _ -> head
  | [] -> ""

let get_middle_elements lst =
  match lst with
  | [] | [ _ ] -> []
  | _first :: middle -> (
      match List.rev middle with
      | [] | [ _ ] -> []
      | _last :: middle_rev -> List.rev middle_rev)

let get_last_element lst =
  match List.rev lst with
  | [] -> ""
  | last :: _ -> last

let update_string_at_cursor (cur_pos : int * int) (original_string : string)
    (paste_string : string list) : string list =
  match List.length paste_string with
  | 0 -> [ original_string ]
  | 1 ->
      [
        String.sub original_string 0 (tuple_get_second cur_pos)
        ^ get_first_element paste_string
        ^ String.sub original_string (tuple_get_second cur_pos)
            (String.length original_string - tuple_get_second cur_pos);
      ]
  | _ ->
      let before = String.sub original_string 0 (tuple_get_second cur_pos) in
      let after =
        String.sub original_string (tuple_get_second cur_pos)
          (String.length original_string - tuple_get_second cur_pos)
      in
      (before ^ get_first_element paste_string)
      :: get_middle_elements paste_string
      @ [ get_last_element paste_string ^ after ]

let rec update_string_list (index : int) cursor_pos content
    (string_list : string list) : string list =
  match string_list with
  | [] -> failwith "Index out of bounds"
  | h :: t ->
      if index = 0 then update_string_at_cursor cursor_pos h content @ t
      else h :: update_string_list (index - 1) cursor_pos content t

let write_string_list_to_file filename string_list =
  let output_channel = open_out filename in
  List.iter (fun s -> output_string output_channel (s ^ "\n")) string_list;
  close_out output_channel

let copy_string_cmd (s : editor_state) =
  if !select_flag = true then (
    clip_board := convert_selection_to_string_list s;
    write_string_list_to_file "clipboard.txt" !clip_board;
    s)
  else s

let helper s =
  let write_int_to_file (filename : string) (tuple : int * int) : unit =
    let oc = open_out filename in
    let x, y = tuple in
    let content = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")\n" in
    output_string oc content;
    close_out oc
  in
  write_int_to_file "output.txt" s.cursor_pos

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
      let row_pos = if y - 3 > rows then rows else max 0 (y - 3) in
      let cols_in_row = String.length (get_nth_elm s.text row_pos) - 1 in
      let col_pos = if x > cols_in_row then cols_in_row else x in
      let newstate =
        if !select_flag = false then
          {
            text = s.text;
            cursor_pos = s.cursor_pos;
            selection_start = Some s.cursor_pos;
            selection_end = Some (row_pos, col_pos);
          }
        else
          {
            text = s.text;
            cursor_pos = (row_pos, col_pos);
            selection_start = Some (row_pos, col_pos);
            selection_end = Some (row_pos, col_pos);
          }
      in
      select_flag := not !select_flag;
      helper newstate;
      newstate
  | _ -> s

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

let insert_char_cmd (s : editor_state) (c : char) =
  if !select_flag = false then
    move_cursor_cmd RIGHT
      (insert_str (is_last_insert_space s)
         (tuple_get_first s.cursor_pos)
         (tuple_get_second s.cursor_pos)
         (String.make 1 c))
  else
    let news = delete_char_cmd s in
    move_cursor_cmd RIGHT
      (insert_str
         (is_last_insert_space news)
         (tuple_get_first news.cursor_pos)
         (tuple_get_second news.cursor_pos)
         (String.make 1 c))

let save_file_cmd (s : editor_state) (name : string) =
  save_file s name;
  s

let newline_cmd (s : editor_state) =
  if !select_flag = false then insert_newline s
  else
    let s = delete_char_cmd s in
    insert_newline s

let paste_string_cmd (s : editor_state) =
  if !select_flag = false then
    let new_context =
      update_string_list
        (tuple_get_first s.cursor_pos)
        s.cursor_pos !clip_board s.text
    in
    if List.length !clip_board = 1 then
      {
        text = new_context;
        cursor_pos =
          ( tuple_get_first s.cursor_pos + List.length !clip_board - 1,
            tuple_get_second s.cursor_pos
            + String.length (get_last_element !clip_board) );
        selection_start = s.selection_start;
        selection_end = s.selection_end;
      }
    else
      {
        text = new_context;
        cursor_pos =
          ( tuple_get_first s.cursor_pos + List.length !clip_board - 1,
            String.length (get_last_element !clip_board) );
        selection_start = s.selection_start;
        selection_end = s.selection_end;
      }
  else
    let s = delete_char_cmd s in
    let new_context =
      update_string_list
        (tuple_get_first s.cursor_pos)
        s.cursor_pos !clip_board s.text
    in
    {
      text = new_context;
      cursor_pos =
        ( tuple_get_first s.cursor_pos + List.length !clip_board - 1,
          String.length (get_last_element !clip_board) );
      selection_start = s.selection_start;
      selection_end = s.selection_end;
    }
