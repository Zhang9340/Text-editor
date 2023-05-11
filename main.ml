open Notty_unix
open Editor
open Text_editor
open Ui

let data_dir_prefix = "data" ^ Filename.dir_sep

let read_file () =
  print_endline "\n\nPlease enter the name of the file you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ""
  | file_name -> data_dir_prefix ^ file_name ^ ".txt"

let init_state = read_file () |> load_file
(* let image = Ui.convert_state init_state *)

(*editing page*)
let rec update t (s : editor_state) (command : cmd) =
  let newstate =
    match command with
    | INIT -> s
    | LEFT | RIGHT | UP | DOWN -> move_cursor_cmd command s
    | INSERT c -> insert_char_cmd s c
    | DELETE -> delete_char_cmd s
    | SAVE name -> save_file_cmd s name
    | MousePressLeft (x, y) -> move_cursor_cmd (MousePressLeft (x, y)) s
    | PASTE -> paste_string_cmd s
    | NEWLINE -> newline_cmd s
    | COPY -> copy_string_cmd s
    | _ -> failwith "not implemented"
  in
  let newimg = Ui.convert_state newstate in
  Term.image t newimg;
  loop t newstate newimg

and loop t (s : editor_state) img =
  match Term.event t with
  | `Key (`ASCII 'P', [ `Ctrl ]) -> update t s PASTE
  | `Key (`ASCII 'C', [ `Ctrl ]) -> update t s COPY
  | `Key (`Enter, _) -> update t s NEWLINE
  | `Key (`Arrow `Left, _) -> update t s LEFT
  | `Key (`Arrow `Right, _) -> update t s RIGHT
  | `Key (`Arrow `Up, _) -> update t s UP
  | `Key (`Arrow `Down, _) -> update t s DOWN
  | `Key (`ASCII chr, _) -> update t s (INSERT chr)
  | `Key (`Backspace, _) -> update t s DELETE
  | `Key (`Escape, _) ->
      save_file s "test.txt";
      ()
  | `Mouse (`Press `Left, (x, y), _) -> update t s (MousePressLeft (x, y))
  | _ -> loop t s img

let t = Term.create ()

let main () =
  update t init_state INIT;
  Term.release t

let () = main ()
