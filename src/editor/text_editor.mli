type editor_state = {
  text : string list;
  cursor_pos : int * int;
  selection_start : (int * int) option;
  selection_end : (int * int) option;
}

val create_editor_state : unit -> editor_state

val load_file : string -> editor_state
(** [load_file f] reads in a .txt file from the current directory with the name
    the same as string f, and show the file as a editor_state. Every line is
    shown as an individual string, and the whole file is shown as a string list. *)

val save_file : editor_state -> string -> unit
(** [save_file s f] saves the current editor_state s as a new file with the name
    the same as string f in the current directory. *)

val get_nth_elm : string list -> int -> string
(** [get_nth_elm s pos] get the number [pos]th element, which is a string, in
    the editor_state [s].*)

val select_text : editor_state -> int -> int -> int -> int -> editor_state
(** [select_text s sr sc er ec] selects text in editor_state from "row start
    position" sr and "column start position" sc (inclusive), to "row end
    position" er and "column end position" ec (inclusive). Returns the new
    editor_state. *)

val replace_str : editor_state -> string -> editor_state
(** [replace_str s str] returns a new editor_state after replacing the text from
    index selection_start (inclusive) to index selection_end (exclusive) with
    string str. If multiple lines are selected, then replace all the select
    words with the new string, and merge the multiples lines that are selected. *)

val insert_str : editor_state -> int -> int -> string -> editor_state
(** [insert_str s r c str] returns a new editor_state after inserting the text
    string str within at position row r and column cle. *)

val move_cursor : editor_state -> int * int -> editor_state
(** [move_cursor s (r c)] changes the cursor position by adding the row offset r
    and the column offset c to the current cursor position. *)

val word_count : editor_state -> int
(** [word_count s] returns the word count of the editor_state s*)

val scapitalize : editor_state -> string list
(** [capitalize s1] returns a new state s2 with all of its ascii characters
    capitalized*)

val sfold : editor_state -> (string -> string) -> string list

(** [fold s f] takes in a state s and a user function (string -> string), and
    returns a new state with f applied to all of the columns of state s*)

val is_last_insert_space : editor_state -> editor_state

val delete : editor_state -> editor_state
(** [fold s f] takes in a state s and a user function (string -> string), and
    returns a new state with f applied to all of the columns of state s*)

val delete_selection : editor_state -> editor_state
val update_in_one_row : editor_state -> string -> int -> int -> int -> string
