open OUnit2
open Editor
open Text_editor

let print_string_list my_list =
  let str_list = List.map (fun x -> "\"" ^ x ^ "\"") my_list in
  let str = String.concat "\n " str_list in
  "\n[" ^ str ^ "\n]"

(*[print_editor_state] is helper function that transfer editor module into
  printable stirng*)
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

(*[select_text_test name state sr sc er ec] is the helper function to test the
  select_text function*)
let select_text_test (name : string) (state : editor_state) (sr : int)
    (sc : int) (er : int) (ec : int) (expected_output : editor_state) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (select_text state sr sc er ec)
    ~printer:print_editor_state

(*[move_cursor_text name state pos expected_output] is the helper function to
  test the move_cursor function*)
let move_cursor_test (name : string) (state : editor_state) (pos : int * int)
    (expected_output : editor_state) : test =
  name >:: fun _ ->
  assert_equal expected_output (move_cursor state pos)
    ~printer:print_editor_state

(*[word_count_test name state expected_output] is the helper function to test
  the word_count function*)
let word_count_test (name : string) (state : editor_state)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (word_count state) ~printer:string_of_int

let delete_test (name : string) (state : editor_state)
    (expected_output : editor_state) : test =
  name >:: fun _ ->
  assert_equal expected_output (delete state) ~printer:print_editor_state

let update_in_one_row_test (name : string) (state : editor_state) (s : string)
    (start_pos_r : int) (start_pos_c : int) (end_pos_c : int)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (update_in_one_row state s start_pos_r start_pos_c end_pos_c)
    ~printer:(fun x -> x)

let editor_test =
  [
    load_file_test "Test the file is corrected loaded" "hello.txt"
      {
        text =
          [
            "Hello world! ";
            "Hello world! Hello world! Hello world! ";
            "Hello world! ";
            "Hello world! Hello world! ";
          ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      };
    load_file_test "Test the file is corrected loaded 2" "hellowithspace.txt"
      {
        text = [ "Hello "; " "; "hello "; " "; "Hello " ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      };
    update_in_one_row_test "update the middle letter"
      {
        text = [ "ABC" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      "E" 0 1 1 "AEC";
    select_text_test "Test the select_text function with input 0, 0, 0, 5"
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
    select_text_test
      "Test select_text with invalid positions (-1, 0) and (1, 100)"
      {
        text = [ "Hello world!" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      (-1) 0 1 100
      {
        text = [ "Hello world!" ];
        cursor_pos = (0, 0);
        selection_start = Some (0, 0);
        selection_end = Some (0, 11);
      };
    select_text_test "Test select_text with first position of the third line"
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      3 (-1) 3 0
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (0, 0);
        selection_start = Some (3, 0);
        selection_end = Some (3, 0);
      };
    move_cursor_test "Test the move_cursor function with cursor position (1,3)"
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
    move_cursor_test
      "Test the move_cursor function with cursor position (-1,20)"
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
      (-1, 20)
      {
        text =
          [
            "Hello world!";
            "Hello world!Hello world!Hello world!";
            "Hello world!";
            "Hello world!Hello world!";
          ];
        cursor_pos = (0, 11);
        selection_start = None;
        selection_end = None;
      };
    replace_str_test
      "test the replace_str fucntion by replacing the letter with 'replace' "
      {
        text =
          [
            "Hello world!";
            "Hello world!Hello\n       world!Hello world!";
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
            "Hello world!Hello\n       world!Hello world!";
            "Hello world!";
            "Hello world!Hello world!";
          ];
        cursor_pos = (0, 0);
        selection_start = Some (0, 0);
        selection_end = Some (0, 5);
      };
    replace_str_test
      "test the replace_str fucntion by replacing the letter with 'abc' "
      {
        text =
          [
            "Hello world!";
            "Hello world!Hello world!Hello world!";
            "Hello world!";
            "Hello world!Hello world!";
          ];
        cursor_pos = (0, 0);
        selection_start = Some (1, 30);
        selection_end = Some (1, 35);
      }
      "abc"
      {
        text =
          [
            "Hello world!";
            "Hello world!Hello world!Hello abc!";
            "Hello world!";
            "Hello world!Hello world!";
          ];
        cursor_pos = (1, 30);
        selection_start = Some (1, 30);
        selection_end = Some (1, 35);
      };
    replace_str_test
      "test the replace_str fucntion (replacing multiple lines) by replacing \
       the first line and the first five letters with 'AAAAA' "
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
        selection_end = Some (1, 5);
      }
      "AAAAA"
      {
        text =
          [
            "AAAAA world!Hello world!Hello world!";
            "Hello world!";
            "Hello world!Hello world!";
          ];
        cursor_pos = (0, 0);
        selection_start = Some (0, 0);
        selection_end = Some (1, 5);
      };
    replace_str_test
      "test the replace_str fucntion (replacing multiple lines) by replacing \
       the whole file with 'AAAAA' "
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
        selection_end = Some (3, 24);
      }
      "AAAAA"
      {
        text = [ "AAAAA" ];
        cursor_pos = (0, 0);
        selection_start = Some (0, 0);
        selection_end = Some (3, 24);
      };
    word_count_test "Count the number of words in file hello.txt."
      {
        text =
          [
            "Hello world!";
            "Hello world! Hello world! Hello world!";
            "Hello world!";
            "Hello world! Hello world!";
          ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      14;
    word_count_test "Count the number of words in an empty file."
      {
        text = [];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      0;
    delete_test "delete the middle letter"
      {
        text = [ "abc" ];
        cursor_pos = (0, 1);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "bc" ];
        cursor_pos = (0, 0);
        selection_start = Some (0, 0);
        selection_end = Some (0, 1);
      };
    delete_test "delete the first letter of the second to last line"
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (3, 0);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "1abc"; "2def"; "3ghi"; "jkl"; "5mno" ];
        cursor_pos = (3, 0);
        selection_start = Some (3, 0);
        selection_end = Some (3, 0);
      };
    delete_test "delete the first letter of the third to last line"
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (2, 0);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "1abc"; "2def"; "3ghi"; "jkl"; "5mno" ];
        cursor_pos = (2, 0);
        selection_start = Some (2, 0);
        selection_end = Some (2, 0);
      };
    delete_test "delete the first letter of the third to last line (8 elements)"
      {
        text =
          [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno"; "6pqr"; "7stu"; "8vwm" ];
        cursor_pos = (4, 0);
        selection_start = None;
        selection_end = None;
      }
      {
        text =
          [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno"; "6pqr"; "7stu"; "8vwm" ];
        cursor_pos = (4, 0);
        selection_start = Some (4, 0);
        selection_end = Some (4, 0);
      };
  ]

let additional_editor_tests =
  [
    (* Test move_cursor with an invalid position *)
    move_cursor_test "Test move_cursor with invalid position (-1, 3)"
      {
        text = [ "Hello world!" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      (-1, 3)
      {
        text = [ "Hello world!" ];
        cursor_pos = (0, 3);
        selection_start = None;
        selection_end = None;
      };
    (* Test move_cursor with position beyond the end of the line *)
    move_cursor_test
      "Test move_cursor with position beyond the end of the line (0, 20)"
      {
        text = [ "Hello world!" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      (0, 20)
      {
        text = [ "Hello world!" ];
        cursor_pos = (0, 11);
        (* Set cursor to end of the line *)
        selection_start = None;
        selection_end = None;
      }
    (* Test select_text with invalid positions *)

    (* Test replace_str with no selection replace_str_test "Test replace_str
       with no selection" { text = [ "Hello world!" ]; cursor_pos = (0, 0);
       selection_start = None; selection_end = None; } "replace" { text = [
       "Hello world!" ]; cursor_pos = (0, 0); selection_start = None;
       selection_end = None; }; *);
  ]

(*[capitalize_test] is the helper function to test the scapitalize function*)
let capitalize_test (name : string) (state : editor_state)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (scapitalize state) ~printer:print_string_list

(*[fold_test] is the helper function to test the sfold function*)
let fold_test (name : string) (state : editor_state)
    (fold_func : string -> string) (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (sfold state fold_func)
    ~printer:print_string_list

let fold_and_capitalize_tests =
  [
    fold_test "Test fold function with sfold"
      {
        text = [ "hello world!"; "this is a test"; "goodbye world!" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      String.uppercase_ascii
      [ "HELLO WORLD!"; "THIS IS A TEST"; "GOODBYE WORLD!" ];
    fold_test "Test fold function with reverse function"
      {
        text = [ "hello world!"; "this is a test"; "goodbye world!" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      String.capitalize_ascii
      [ "Hello world!"; "This is a test"; "Goodbye world!" ];
    capitalize_test "Test capitalize function on a sample text"
      {
        text = [ "hello world!"; "this is a test"; "goodbye world!" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      [ "HELLO WORLD!"; "THIS IS A TEST"; "GOODBYE WORLD!" ];
  ]

let is_last_insert_space_test (name : string) (state : editor_state)
    (expected_output : editor_state) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (is_last_insert_space state)
    ~printer:print_editor_state

let is_last_insert_space_tests =
  [
    is_last_insert_space_test "test_single_line_begining"
      {
        text = [ "hello" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "hello" ];
        cursor_pos = (0, 1);
        selection_start = None;
        selection_end = None;
      };
    is_last_insert_space_test "test_single_line_end"
      {
        text = [ "hello" ];
        cursor_pos = (0, 5);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "hello " ];
        cursor_pos = (0, 6);
        selection_start = None;
        selection_end = None;
      };
    is_last_insert_space_test "test_multiple_lines_beginning"
      {
        text = [ "hello"; "world" ];
        cursor_pos = (1, 0);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "hello"; "world" ];
        cursor_pos = (1, 1);
        selection_start = None;
        selection_end = None;
      };
    is_last_insert_space_test "test_multiple_lines_end"
      {
        text = [ "hello"; "world" ];
        cursor_pos = (0, 5);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "hello "; "world" ];
        cursor_pos = (0, 6);
        selection_start = Some (0, 1);
        selection_end = Some (0, 1);
      };
  ]

let suite =
  "test suite for Text_editor"
  >::: List.flatten
         [
           editor_test;
           additional_editor_tests;
           fold_and_capitalize_tests;
           is_last_insert_space_tests;
         ]

let _ = run_test_tt_main suite
