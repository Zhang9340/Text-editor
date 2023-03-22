open OUnit2
open Editor
open Text_editor

(* let compare_editor_state (expected : editor_state) (s : editor_state) = s =
   expected

   let get_text s = s.text *)
let print_string_list my_list =
  let str_list = List.map (fun x -> "\"" ^ x ^ "\"") my_list in
  let str = String.concat "\n " str_list in
  "\n[" ^ str ^ "\n]"

let print_editor_state state =
  let text_str = print_string_list state.text in
  let cursor_pos_str =
    Printf.sprintf "(%d, %d)" (fst state.cursor_pos) (snd state.cursor_pos)
  in
  let selection_start_str =
    match state.selection_start with
    | Some (start_x, start_y) -> Printf.sprintf "(%d, %d)" start_x start_y
    | None -> "None"
  in
  let selection_end_str =
    match state.selection_end with
    | Some (end_x, end_y) -> Printf.sprintf "(%d, %d)" end_x end_y
    | None -> "None"
  in
  Printf.sprintf
    "\ntext: %s\n cursor_pos: %s\n selection_start: %s\n selection_end: %s\n"
    text_str cursor_pos_str selection_start_str selection_end_str

let load_file_test_text (name : string) (filename : string)
    (expected_output : editor_state) : test =
  name >:: fun _ ->
  assert_equal expected_output (load_file filename) ~printer:print_editor_state

let editor_test =
  [
    load_file_test_text "Test the file is corrected loaded" "hello.txt"
      {
        text =
          [
            "Hello world!";
            "Hello world!Hello world!Hello world!";
            "Hello world!";
            "Hello world!Hello world!";
          ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      };
  ]

let suite = "test suite for Text_editor" >::: List.flatten [ editor_test ]
let _ = run_test_tt_main suite
