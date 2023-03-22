module Text : sig
  type editor_state = {
    text : string list;
    cursor_pos : int * int;
    selection_start : int option * int option;
    selection_end : int option * int option;
  }

  val create_editor_state : unit -> editor_state
  val get_text : editor_state -> string list
  val load_file : string -> editor_state
  val text_format : editor_state -> string
  val save_file : editor_state -> string -> unit
  val sublist : int -> int -> 'a list -> 'a list
  val update_in_one_row : editor_state -> string -> int -> int -> int -> string
  val update_row : editor_state -> string -> string
  val update_all_rows : editor_state -> string -> string list
  val option_tuple_get_int : 'a option * 'b option -> 'a * 'b
  val insert_str : editor_state -> string -> editor_state
  val select_text : editor_state -> int -> int -> int -> int -> editor_state
end
