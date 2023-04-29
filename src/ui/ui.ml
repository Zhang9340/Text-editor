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
  | INSERT
  | DELTET
  | REPLACE
  | INIT

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

  let rec convert (c : string list) (inc : int) =
    match c with
    | [] -> I.empty
    | h :: t ->
        if inc = tuple_get_first s.cursor_pos then
          I.(string_to_image h true <-> convert t (inc + 1))
        else I.(string_to_image h false <-> convert t (inc + 1))
  in

  convert s.text 0

let write_int_to_file (filename : string) (tuple : int * int) : unit =
  let oc = open_out filename in
  let x, y = tuple in
  let content = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")\n" in
  output_string oc content;
  close_out oc

let helper s = write_int_to_file "output.txt" s.cursor_pos

(* input editor state and direction return new editor state*)
let move_cursor_cmd (s : editor_state) (dir : cmd) =
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
