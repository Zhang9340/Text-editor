type editor_state = {
  text : string list;
  cursor_pos : int * int;
  selection_start : int option * int option;
  selection_end : int option * int option;
}

val load_file : string -> editor_state
val save_file : editor_state -> string -> unit
val replace_str : editor_state -> string -> editor_state
val select_text : editor_state -> int -> int -> int -> int -> editor_state
val move_cursor : editor_state -> int * int -> editor_state
