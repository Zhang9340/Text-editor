val select_flag : bool ref
(** [select_flag] shows whether the cursor is at the end of the line. *)

(** [cmd] Direction command. Correspond to keyboard actions. *)
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

val get_select_start : Editor.Text_editor.editor_state -> int * int
(** [get_select_start] get the start position of the selection, change tuple
    option into tuple. *)

val get_select_end : Editor.Text_editor.editor_state -> int * int
(** [get_select_end] get the end position of the selection, change tuple option
    into tuple. *)

val is_select : Editor.Text_editor.editor_state -> int -> int -> bool
(** [is_select] *)

val loop :
  Editor.Text_editor.editor_state ->
  string ->
  int ->
  Notty.image ->
  bool ->
  int ->
  Notty.image
(** [loop s str i img flag nrow] loop through the string and build the whole
    text file image by turning each character into a single image. Starting from
    an empty image *)

val string_to_image :
  Editor.Text_editor.editor_state -> string -> bool -> int -> Notty.image
(** [string_to_image str] turn a string (in the text) into Notty images. *)

val header_to_image : string -> Notty.image
(** [header_to_image str] turn the header, which is a string, into a Notty
    image. *)

val display_range : Editor.Text_editor.editor_state -> int * int
(** [display_range s] determines the display size of the user interface. Now the
    user interface is displaying in the terminal, and there will be at most 40
    lines on the screen. *)

val convert :
  Editor.Text_editor.editor_state ->
  string list ->
  int ->
  Notty.image ->
  Notty.image
(** [convert_state s] converts every line of the text in the editor_state [s]
    into an individual Notty image. *)

val convert_state : Editor.Text_editor.editor_state -> Notty.image
(** [convert_state s] converts the current editor_state into a Notty image,
    including the header and the end line. *)

val helper : Editor.Text_editor.editor_state -> unit
(** [helper s] returns the current cursor position to a file called output.txt
    in the current directory. *)

val move_cursor_cmd :
  cmd -> Editor.Text_editor.editor_state -> Editor.Text_editor.editor_state
(* [move_cursor_cmd] takes in editor state and direction, return new an editor
   state with the cursor moved to [dir]. *)

val insert_char_cmd :
  Editor.Text_editor.editor_state -> char -> Editor.Text_editor.editor_state
(* [insert_char_cmd] takes in editor state and a character, return new an editor
   state with the character inserted at the current position. *)

val delete_char_cmd :
  Editor.Text_editor.editor_state -> Editor.Text_editor.editor_state
(* [insert_char_cmd] takes in editor state, return new an editor state with the
   character at the current position deleted. *)

val save_file_cmd :
  Editor.Text_editor.editor_state -> string -> Editor.Text_editor.editor_state
(* [save_file_cmd] takes in editor state and a string, generate a new txt file
   in the current directory, showing the text field of the current editor_state
   s. *)
