type editor_state = {
  text : string list;
  cursor_pos : int * int;
  selection_start : (int * int) option;
  selection_end : (int * int) option;
}

let get_text state = state.text

let load_file filename =
  let chan = open_in filename in
  let rec loop acc =
    try
      let line = input_line chan in
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

(* [sublist begin end list] get the sublist from begin (inclusive) to end
   (exclusive) *)
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

let update_in_one_row state s start_pos_r start_pos_c end_pos_c =
  let stext = List.nth (get_text state) start_pos_r in
  let prefix = String.sub stext 0 start_pos_c in
  let suffix = String.sub stext end_pos_c (String.length stext - end_pos_c) in
  prefix ^ s ^ suffix

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

let update_all_rows state r =
  match (state.selection_start, state.selection_end) with
  | Some (start_pos_r, _), Some (end_pos_r, _) ->
      let prefix = sublist 0 start_pos_r (get_text state) in
      (* let middle = sublist (start_pos_r + 1) end_pos_r (get_text state) in *)
      let suffix =
        sublist (end_pos_r + 1)
          (List.length (get_text state) - end_pos_r + 1)
          (get_text state)
      in
      prefix @ [ r ] @ suffix
  | _ -> failwith "fail to update all rows"

let option_tuple_get_int t =
  match t with
  | Some (a, b) -> (a, b)
  | _ -> failwith "internal error"

let select_text state start_pos_r start_pos_c end_pos_r end_pos_c =
  {
    state with
    selection_start = Some (start_pos_r, start_pos_c);
    selection_end = Some (end_pos_r, end_pos_c);
  }

let replace_str state s =
  let text = s |> update_row state |> update_all_rows state in
  { state with text; cursor_pos = option_tuple_get_int state.selection_start }

let insert_str state pos_r pos_c s =
  let file = select_text state pos_r pos_c pos_r pos_c in
  replace_str file s

let tuple_get_first t =
  match t with
  | r, _ -> r

let tuple_get_second t =
  match t with
  | _, c -> c

let move_cursor state (offset_r, offset_c) =
  let new_pos_r =
    max 0
      (min (List.length state.text)
         (tuple_get_first state.cursor_pos + offset_r))
  in
  let new_pos_c =
    max 0
      (min (List.length state.text)
         (tuple_get_second state.cursor_pos + offset_c))
  in
  {
    state with
    cursor_pos = (new_pos_r, new_pos_c);
    selection_start = None;
    selection_end = None;
  }
