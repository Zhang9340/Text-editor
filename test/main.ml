open OUnit2
open Editor
open Text_editor

(* Test Strategy : The majority of the text editor functionality is tested using
   unit tests. These tests cover the following areas:

   Create, Load, and Save: Tests for the proper creation of a new editor state,
   loading from a file, and saving to a file. Update: Tests for updating the
   editor state with new user input. Select: Tests for handling text selection
   (single and multiple lines). Move: Tests for moving the cursor within the
   editor. Edit Text: Tests for inserting, deleting, and replacing text. Count:
   Tests for counting the occurrences of a given character or word in the text.
   Fold and Capitalize: Tests for folding (applying a function to all lines) and
   capitalizing the text. Is Last Insert Space: Tests for identifying if the
   last inserted character is a space. Insert Newline: Tests for inserting a
   newline character at various cursor positions. Convert Selection to String
   List: Tests for converting a text selection into a list of strings. 2.2.
   Manual Testing

   In addition to the unit tests for editor state module, manual testing is
   performed to ensure the correctness of the text editor's user interface and
   user experience. This includes:

   Visual inspection of the editor: Ensuring proper rendering of text, cursor,
   and selections. Responsiveness: Testing the performance and responsiveness of
   the editor when handling large files or complex operations. Usability:
   Verifying that the user interface is intuitive and easy to use, with clear
   and informative error messages when needed. Argument for Correctness The
   correctness of the Text Editor implementation is supported by the combination
   of automated tests and manual testing. The automated tests are designed to
   cover a wide range of possible scenarios, inputs, and edge cases. By passing
   these tests, we can confidently say that the implementation is likely to be
   correct for the majority of use cases.

   However, automated tests may not cover every possible situation. Manual
   testing helps to fill in the gaps by allowing for a more comprehensive
   evaluation of the editor's usability, performance, and functionality. By
   combining both approaches, we can ensure a high level of confidence in the
   Text Editor's correctness and quality. *)
let print_string_list my_list =
  let str_list = List.map (fun x -> "\"" ^ x ^ "\"") my_list in
  let str = String.concat "\n " str_list in
  "\n[" ^ str ^ "\n]"

(** [print_editor_state] is helper function that transfer editor module into
    printable stirng *)
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

(** [create_editor_state_test] is the helper function to test the load_file
    function *)
let create_editor_state_test (name : string) (expected_output : editor_state) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (create_editor_state ())
    ~printer:print_editor_state

(** [load_file_test] is the helper function to test the load_file function *)
let load_file_test (name : string) (filename : string)
    (expected_output : editor_state) : test =
  name >:: fun _ ->
  assert_equal expected_output (load_file filename) ~printer:print_editor_state

let creat_load_save_test =
  [
    create_editor_state_test "create a new editor_state"
      {
        text = [];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      };
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
  ]

let sublist_test (name : string) (b : int) (e : int) (l : 'a list)
    (expected_output : 'a list) : test =
  name >:: fun _ ->
  assert_equal expected_output (sublist b e l) ~printer:print_string_list

(** [update_in_one_row_test name state s start_pos_r start_pos_c end_pos_c expected_output]
    is the helper function to test the update_in_one_row function *)
let update_in_one_row_test (name : string) (state : editor_state) (s : string)
    (start_pos_r : int) (start_pos_c : int) (end_pos_c : int)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (update_in_one_row state s start_pos_r start_pos_c end_pos_c)
    ~printer:(fun x -> x)

(** [update_row_test name state s expected_output] is the helper function to
    test the update_row function *)
let update_row_test (name : string) (state : editor_state) (s : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (update_row state s) ~printer:(fun x -> x)

(** [update_all_rows_test name state r expected_output] is the helper function
    to test the update_all_rows function *)
let update_all_rows_test (name : string) (state : editor_state) (r : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (update_all_rows state r)
    ~printer:print_string_list

let update_test =
  [
    sublist_test "sublist from index 1 to 3" 1 3
      [ "A"; "B"; "C"; "D"; "E" ]
      [ "B"; "C" ];
    sublist_test "sublist from index 0 to 2" 0 2
      [ "A"; "B"; "C"; "D"; "E" ]
      [ "A"; "B" ];
    sublist_test "sublist from index 3 to 3" 3 3 [ "A"; "B"; "C"; "D"; "E" ] [];
    sublist_test "sublist with empty list" 1 2 [] [];
    update_in_one_row_test "update the middle letter"
      {
        text = [ "ABC" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      "E" 0 1 1 "AEBC";
    update_in_one_row_test "update the middle letter"
      {
        text = [ "ABC" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      "E" 0 1 2 "AEC";
    update_in_one_row_test "update the last letter"
      {
        text = [ "ABC" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      "E" 0 2 3 "ABE";
    update_row_test "update single row selection"
      {
        text = [ "ABC"; "DEF"; "GHI" ];
        cursor_pos = (0, 0);
        selection_start = Some (1, 1);
        selection_end = Some (1, 2);
      }
      "X" "DXF";
    update_row_test "update multi row selection"
      {
        text = [ "ABC"; "DEF"; "GHI" ];
        cursor_pos = (0, 0);
        selection_start = Some (1, 2);
        selection_end = Some (2, 1);
      }
      "X" "DEXHI";
    update_all_rows_test "update all rows with single row selection"
      {
        text = [ "ABC"; "DEF"; "GHI" ];
        cursor_pos = (0, 0);
        selection_start = Some (1, 1);
        selection_end = Some (1, 2);
      }
      "XYZ" [ "ABC"; "XYZ"; "GHI" ];
    update_all_rows_test "update all rows with multi row selection (WRONG)"
      {
        text = [ "ABC"; "DEF"; "GHI" ];
        cursor_pos = (0, 0);
        selection_start = Some (0, 2);
        selection_end = Some (2, 1);
      }
      "XYZ" [ "XYZ" ];
  ]

(** [select_text_test name state sr sc er ec] is the helper function to test the
    select_text function *)
let select_text_test (name : string) (state : editor_state) (sr : int)
    (sc : int) (er : int) (ec : int) (expected_output : editor_state) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (select_text state sr sc er ec)
    ~printer:print_editor_state

let select_test =
  [
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
    select_text_test
      "Test select_text with start at the first position of the third line and \
       end at the first position of the second line"
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      3 0 2 0
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (0, 0);
        selection_start = Some (3, 0);
        selection_end = Some (2, 0);
      };
    select_text_test
      "Test select_text with start at the first position of the forth line and \
       end at the first position of the second line"
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      4 0 2 0
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (0, 0);
        selection_start = Some (4, 0);
        selection_end = Some (2, 0);
      };
    select_text_test "Test select_text with first position"
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      0 0 0 0
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (0, 0);
        selection_start = Some (0, 0);
        selection_end = Some (0, 0);
      };
  ]

(** [move_cursor_text name state pos expected_output] is the helper function to
    test the move_cursor function *)
let move_cursor_test (name : string) (state : editor_state) (pos : int * int)
    (expected_output : editor_state) : test =
  name >:: fun _ ->
  assert_equal expected_output (move_cursor state pos)
    ~printer:print_editor_state

let move_test =
  [
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
        selection_start = None;
        selection_end = None;
      };
  ]

(** [replace_str_test] is the helper function to test the replace_str function *)
let replace_str_test (name : string) (state : editor_state)
    (replace_string : string) (expected_output : editor_state) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (replace_str state replace_string)
    ~printer:print_editor_state

(** [delete_test name state expected_output] is the helper function to test the
    delete function *)
let delete_test (name : string) (state : editor_state)
    (expected_output : editor_state) : test =
  name >:: fun _ ->
  assert_equal expected_output (delete state) ~printer:print_editor_state

let edit_text_test =
  [
    replace_str_test
      "test the replace_str fucntion by replacing the letter with 'replace' "
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
       the first line with '' "
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
        selection_end = Some (0, 12);
      }
      ""
      {
        text =
          [
            "";
            "Hello world!Hello world!Hello world!";
            "Hello world!";
            "Hello world!Hello world!";
          ];
        cursor_pos = (0, 0);
        selection_start = Some (0, 0);
        selection_end = Some (0, 12);
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
    delete_test "delete the first letter"
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
    delete_test "delete the first letter of the fourth line"
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (3, 1);
        selection_start = Some (3, 0);
        selection_end = Some (3, 1);
      }
      {
        text = [ "1abc"; "2def"; "3ghi"; "jkl"; "5mno" ];
        cursor_pos = (3, 0);
        selection_start = Some (3, 0);
        selection_end = Some (3, 1);
      };
    delete_test "delete the first letter of the last line"
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (4, 1);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "mno" ];
        cursor_pos = (4, 0);
        selection_start = Some (4, 0);
        selection_end = Some (4, 1);
      };
    delete_test "delete the second to last letter of the last line"
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (4, 3);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mo" ];
        cursor_pos = (4, 2);
        selection_start = Some (4, 2);
        selection_end = Some (4, 3);
      };
    delete_test "delete the last letter of the last line"
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno " ];
        cursor_pos = (4, 4);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mn " ];
        cursor_pos = (4, 3);
        selection_start = Some (4, 3);
        selection_end = Some (4, 4);
      };
    delete_test "delete the invalid (zero) letter of the fifth line"
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
    delete_test "delete when cursor at the very beginning"
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (0, 0);
        selection_start = Some (0, 0);
        selection_end = Some (0, 0);
      };
    delete_test "delete invalid"
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (-1, 0);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "1abc"; "2def"; "3ghi"; "4jkl"; "5mno" ];
        cursor_pos = (0, 0);
        selection_start = Some (0, 0);
        selection_end = Some (0, 0);
      };
  ]

(** [word_count_test name state expected_output] is the helper function to test
    the word_count function *)
let word_count_test (name : string) (state : editor_state)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (word_count state) ~printer:string_of_int

let count_test =
  [
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
    word_count_test "Count the number of words in an empty string."
      {
        text = [ "" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      0;
    word_count_test "Count the number of words in an empty file."
      {
        text = [];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      0;
  ]

(*[capitalize_test] is the helper function to test the scapitalize function*)
let capitalize_test (name : string) (state : editor_state)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (capitalize state) ~printer:print_string_list

(*[fold_test] is the helper function to test the sfold function*)
let fold_test (name : string) (state : editor_state)
    (fold_func : string -> string) (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (fold state fold_func) ~printer:print_string_list

let fold_and_capitalize_tests =
  [
    fold_test "Test fold function with String.uppercase_ascii"
      {
        text = [ "hello world!"; "this is a test"; "goodbye world!" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      String.uppercase_ascii
      [ "HELLO WORLD!"; "THIS IS A TEST"; "GOODBYE WORLD!" ];
    fold_test "Test fold function with String.capitalize_ascii"
      {
        text = [ "hello world!"; "this is a test"; "goodbye world!" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      String.capitalize_ascii
      [ "Hello world!"; "This is a test"; "Goodbye world!" ];
    fold_test "Test fold function with String.lowercase_ascii"
      {
        text = [ "Hello world!"; "This is a test"; "Goodbye world!" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      String.lowercase_ascii
      [ "hello world!"; "this is a test"; "goodbye world!" ];
    capitalize_test "Test capitalize function on a sample text"
      {
        text = [ "hello world!"; "this is a test"; "goodbye world!" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      [ "HELLO WORLD!"; "THIS IS A TEST"; "GOODBYE WORLD!" ];
    capitalize_test "Test capitalize function on already capitalized text"
      {
        text = [ "HELLO WORLD!"; "THIS IS A TEST"; "GOODBYE WORLD!" ];
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
    is_last_insert_space_test "test_single_line_end (WRONG)"
      {
        text = [ "hello" ];
        cursor_pos = (0, 4);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "hello" ];
        cursor_pos = (0, 4);
        selection_start = None;
        selection_end = None;
      };
    is_last_insert_space_test
      "not able to insert since it's not the last position"
      {
        text = [ "hello" ];
        cursor_pos = (0, 4);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "hello" ];
        cursor_pos = (0, 4);
        selection_start = None;
        selection_end = None;
      };
    is_last_insert_space_test "test_multiple_lines_end (WRONG)"
      {
        text = [ "hello"; "world" ];
        cursor_pos = (1, 4);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "hello"; "world" ];
        cursor_pos = (1, 4);
        selection_start = None;
        selection_end = None;
      };
  ]

let convert_selection_to_string_list_test (name : string) (state : editor_state)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (convert_selection_to_string_list state)
    ~printer:(fun l -> String.concat "; " l)

let convert_selection_to_string_list_tests =
  [
    convert_selection_to_string_list_test "test_no_selection"
      {
        text = [ "hello" ];
        cursor_pos = (0, 4);
        selection_start = None;
        selection_end = None;
      }
      [];
    convert_selection_to_string_list_test "test_same_line_selection"
      {
        text = [ "hello" ];
        cursor_pos = (0, 4);
        selection_start = Some (0, 1);
        selection_end = Some (0, 3);
      }
      [ "ell" ];
    convert_selection_to_string_list_test "test_multi_line_selection"
      {
        text = [ "hello"; "world"; "goodbye" ];
        cursor_pos = (2, 3);
        selection_start = Some (0, 1);
        selection_end = Some (2, 3);
      }
      [ "ello"; "world"; "good" ];
  ]

let insert_newline_test (name : string) (input : editor_state)
    (expected_output : editor_state) : test =
  name >:: fun _ ->
  assert_equal expected_output (insert_newline input)
    ~printer:print_editor_state

let new_line_tests =
  [
    insert_newline_test "Insert newline at the beginning of the text"
      {
        text = [ "Hello world! " ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ ""; "Hello world! " ];
        cursor_pos = (1, 0);
        selection_start = None;
        selection_end = None;
      };
    insert_newline_test "Insert newline in the middle of a line"
      {
        text = [ "Hello world! " ];
        cursor_pos = (0, 6);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "Hello "; "world! " ];
        cursor_pos = (1, 0);
        selection_start = None;
        selection_end = None;
      };
    insert_newline_test "Insert newline at the end of the text"
      {
        text = [ "Hello world! " ];
        cursor_pos = (0, 13);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "Hello world! "; "" ];
        cursor_pos = (1, 0);
        selection_start = None;
        selection_end = None;
      };
    insert_newline_test "Insert newline in an empty line"
      {
        text = [ "" ];
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ ""; "" ];
        cursor_pos = (1, 0);
        selection_start = None;
        selection_end = None;
      };
    insert_newline_test "Insert newline in a line with multiple spaces"
      {
        text = [ "  Hello   world!  " ];
        cursor_pos = (0, 8);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "  Hello "; "  world!  " ];
        cursor_pos = (1, 0);
        selection_start = None;
        selection_end = None;
      };
    insert_newline_test "Insert newline in a line with special characters"
      {
        text = [ "Hello, world! @#^&*()" ];
        cursor_pos = (0, 12);
        selection_start = None;
        selection_end = None;
      }
      {
        text = [ "Hello, world"; "! @#^&*()" ];
        cursor_pos = (1, 0);
        selection_start = None;
        selection_end = None;
      };
  ]

let suite =
  "test suite for Text_editor"
  >::: List.flatten
         [
           creat_load_save_test;
           update_test;
           select_test;
           move_test;
           edit_text_test;
           count_test;
           fold_and_capitalize_tests;
           is_last_insert_space_tests;
           new_line_tests;
           convert_selection_to_string_list_tests;
         ]

let _ = run_test_tt_main suite
