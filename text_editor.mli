type editor_state = {
  text : string list;
  cursor_pos : int;
  selection_start : int option;
  selection_end : int option;
}

val create_editor_state : unit -> editor_state
val load_file : string -> editor_state
val save_file : editor_state -> string -> unit
val insert_char : editor_state -> char -> editor_state
val delete_char : editor_state -> editor_state
val move_cursor : editor_state -> int -> editor_state
val select_text : editor_state -> int -> int -> editor_state
