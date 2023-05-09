type editor_state = {
  text : string list;
  cursor_pos : int * int;
  selection_start : (int * int) option;
  selection_end : (int * int) option;
}

let create_editor_state () =
  {
    text = [];
    cursor_pos = (0, 0);
    selection_start = None;
    selection_end = None;
  }

let get_text state = state.text

let load_file filename =
  let chan = open_in filename in
  let rec loop acc =
    try
      let line = input_line chan ^ " " in
      loop (line :: acc)
    with End_of_file ->
      close_in chan;
      {
        text = List.rev acc;
        cursor_pos = (0, 0);
        selection_start = None;
        selection_end = None;
      }
  in
  loop []

(* let text_format state = let lst = get_text state in let rec loop ls = match
   ls with | hd :: tl -> if tl == [] then hd else hd ^ "\n" ^ loop tl | [] -> ""
   in loop lst *)

let save_file state filename =
  let chan = open_out filename in
  List.iter (fun line -> output_string chan (line ^ "\n")) (get_text state);
  close_out chan


let rec sublist b e l =
  match l with
  | [] -> []
  (* If the list is empty, return an empty list instead of raising an
     exception *)
  | h :: t ->
      (* If the current index is within the range [b, e), include the current
         element in the result *)
      let head = if b <= 0 && e > 0 then [ h ] else [] in
      (* Recursively extract the sublist from the tail of the list *)
      let tail = sublist (b - 1) (e - 1) t in
      (* Combine the head and tail to form the final result *)
      head @ tail

(*for replace*)
let update_in_one_row state s start_pos_r start_pos_c end_pos_c =
  let stext = List.nth (get_text state) start_pos_r in
  let prefix = String.sub stext 0 start_pos_c in
  let suffix = String.sub stext end_pos_c (String.length stext - end_pos_c) in
  prefix ^ s ^ suffix

(*for replace*)
let update_row state s =
  match (state.selection_start, state.selection_end) with
  | Some (start_pos_r, start_pos_c), Some (end_pos_r, end_pos_c) ->
      if start_pos_r = end_pos_r then
        update_in_one_row state s start_pos_r start_pos_c end_pos_c
      else
        let start_line = List.nth (get_text state) start_pos_r in
        let end_line = List.nth (get_text state) end_pos_r in
        let prefix = String.sub start_line 0 start_pos_c in
        let suffix =
          String.sub end_line end_pos_c (String.length end_line - end_pos_c)
        in
        prefix ^ s ^ suffix
  | _ -> failwith "fail to update row"

(*for replace*)
let update_all_rows state r =
  match (state.selection_start, state.selection_end) with
  | Some (start_pos_r, _), Some (end_pos_r, _) ->
      let prefix = sublist 0 start_pos_r (get_text state) in
      let suffix =
        sublist (end_pos_r + 1)
          (end_pos_r + (List.length (get_text state) - end_pos_r + 1) - 1)
          (get_text state)
      in
      prefix @ [ r ] @ suffix
  | _ -> failwith "fail to update all rows"

let option_tuple_get_int t =
  match t with
  | Some (a, b) -> (a, b)
  | _ -> failwith "internal error"

let rec get_nth_elm (slist : string list) (pos : int) =
  match slist with
  | [] -> failwith "empty list"
  | h :: t -> if pos = 0 then h else get_nth_elm t (pos - 1)

let tuple_get_first t =
  match t with
  | r, _ -> r

let tuple_get_second t =
  match t with
  | _, c -> c

let select_text state start_pos_r start_pos_c end_pos_r end_pos_c =
  let start_r = max 0 (min (List.length state.text - 1) start_pos_r) in
  let start_c =
    max 0 (min (String.length (get_nth_elm state.text start_r) - 1) start_pos_c)
  in
  let end_r = max 0 (min (List.length state.text - 1) end_pos_r) in
  let end_c =
    max 0 (min (String.length (get_nth_elm state.text end_r) - 1) end_pos_c)
  in
  {
    state with
    selection_start = Some (start_r, start_c);
    selection_end = Some (end_r, end_c);
  }

let replace_str state s =
  let text = s |> update_row state |> update_all_rows state in
  { state with text; cursor_pos = option_tuple_get_int state.selection_start }

let insert_str state pos_r pos_c s =
  let file = select_text state pos_r pos_c pos_r pos_c in
  replace_str file s

let delete s =
  let cur_pos = s.cursor_pos in
  let new_s =
    select_text s (tuple_get_first cur_pos)
      (tuple_get_second cur_pos - 1)
      (tuple_get_first cur_pos) (tuple_get_second cur_pos)
  in
  replace_str new_s ""

let delete_selection s =
  let select_start = Option.get s.selection_start in
  let select_end = Option.get s.selection_end in
  let new_s =
    select_text s
      (tuple_get_first select_start)
      (tuple_get_second select_start)
      (tuple_get_first select_end)
      (tuple_get_second select_end)
  in
  replace_str new_s ""

let move_cursor state (offset_r, offset_c) =
  (* let cur_r = tuple_get_first state.cursor_pos + offset_r in *)
  let new_pos_r =
    max 0
      (min
         (List.length state.text - 1)
         (tuple_get_first state.cursor_pos + offset_r))
  in
  let new_pos_c =
    max 0
      (min
         (String.length (get_nth_elm state.text new_pos_r) - 1)
         (tuple_get_second state.cursor_pos + offset_c))
  in
  {
    state with
    cursor_pos = (new_pos_r, new_pos_c);
    selection_start = None;
    selection_end = None;
  }

let count_words_from_string (s : string) : int =
  let is_whitespace ch = ch = ' ' || ch = '\t' || ch = '\n' in
  let rec count_words_helper i in_word count =
    if i >= String.length s then if in_word then count + 1 else count
    else
      let ch = String.get s i in
      if is_whitespace ch then
        if in_word then count_words_helper (i + 1) false (count + 1)
        else count_words_helper (i + 1) false count
      else count_words_helper (i + 1) true count
  in
  count_words_helper 0 false 0

let word_count (s : editor_state) : int =
  let rec helper sl =
    match sl with
    | [] -> 0
    | h :: t -> helper t + count_words_from_string h
  in
  helper s.text

let rec capitalize_line s =
  let len = String.length s in
  let capitalized = String.capitalize_ascii s in
  if len = 0 then ""
  else
    String.make 1 capitalized.[0] ^ capitalize_line (String.sub s 1 (len - 1))

let capitalize (s : editor_state) =
  let rec helper sl =
    match sl with
    | [] -> []
    | h :: t -> capitalize_line h :: helper t
  in
  helper s.text

let fold (s : editor_state) f : string list =
  let rec helper sl =
    match sl with
    | [] -> []
    | h :: t -> f h :: helper t
  in
  helper s.text

let is_last_insert_space (s : editor_state) =
  let curs_row = tuple_get_first s.cursor_pos in
  let curs_col = tuple_get_second s.cursor_pos in
  let selectedrow = get_nth_elm s.text curs_row in
  if String.length selectedrow <= curs_col then
    insert_str s
      (tuple_get_first s.cursor_pos)
      (tuple_get_second s.cursor_pos + 1)
      (String.make 1 ' ')
  else s
