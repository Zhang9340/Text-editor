open OUnit2
open Editor
open Text_editor

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

(*[print_editor_state] is helper function that transfer editor module into
  printable stirng*)

(*[replace_str_test] is the helper function to test the replace_str function*)
let replace_str_test (name : string) (state : editor_state)
    (replace_string : string) (expected_output : editor_state) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (replace_str state replace_string)
    ~printer:print_editor_state
(*[load_file_test] is the helper function to test the load_file function*)

let load_file_test (name : string) (filename : string)
    (expected_output : editor_state) : test =
  name >:: fun _ ->
  assert_equal expected_output (load_file filename) ~printer:print_editor_state

let select_text (name : string) (state : editor_state) (sr : int) (sc : int)
    (er : int) (ec : int) (expected_output : editor_state) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (select_text state sr sc er ec)
    ~printer:print_editor_state
(*[select_test name state sr sc er ec] is the helper function to test the
  load_file function*)

let move_cursor_text (name : string) (state : editor_state) (pos : int * int)
    (expected_output : editor_state) : test =
  name >:: fun _ ->
  assert_equal expected_output (move_cursor state pos)
    ~printer:print_editor_state

let editor_test =
  [
    load_file_test "Test the file is corrected loaded" "hello.txt"
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
    select_text "Test the select_text function with input 0 ,0 ,0,5"
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
      }
      0 0 0 5
      {
        text =
          [
            "Hello world!";
            "Hello world!Hello world!Hello world!";
            "Hello world!";
            "Hello world!Hello world!";
          ];
        cursor_pos = (0, 0);
        selection_start = Some (0, 0);
        selection_end = Some (0, 5);
      };
    move_cursor_text "Test the move_cursor function with cursor position (1,3)"
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
      }
      (1, 3)
      {
        text =
          [
            "Hello world!";
            "Hello world!Hello world!Hello world!";
            "Hello world!";
            "Hello world!Hello world!";
          ];
        cursor_pos = (1, 3);
        selection_start = None;
        selection_end = None;
      };
    replace_str_test "string"
      {
        text =
          [
            "Hello world!";
            "Hello world!Hello world!Hello world!";
            "Hello world!";
            "Hello world!Hello world!";
          ];
        cursor_pos = (0, 0);
        selection_start = Some (0, 0);
        selection_end = Some (0, 5);
      }
      "replace"
      {
        text =
          [
            "replace world!";
            "Hello world!Hello world!Hello world!";
            "Hello world!";
            "Hello world!Hello world!";
          ];
        cursor_pos = (0, 0);
        selection_start = Some (0, 0);
        selection_end = Some (0, 5);
      };
  ]

let suite = "test suite for Text_editor" >::: List.flatten [ editor_test ]
let _ = run_test_tt_main suite
