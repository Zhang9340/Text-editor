open Notty
open Notty_unix

let example = [ "a"; "qwer"; "c" ]

let string_to_image s =
  let len = String.length s in
  let rec loop i img =
    if i < len then
      let ch = s.[i] in
      let ch_img = I.uchar (A.fg A.blue) (Uchar.of_char ch) 1 1 in
      loop (i + 1) I.(img <-> ch_img)
    else img
  in
  loop 0 I.empty

let rec convert (c : string list) =
  match c with
  | [] -> I.empty
  | h :: t -> I.(string_to_image h <|> convert t)

(* let () = let img = convert example in Term.image_size (Term.create ()) img |>
   Printf.printf "Image size: %d x %d\n%!"; Term.image (Term.create ()) img *)

let test =
  let img = convert example in
  let term = Term.create () in
  Term.image term img;
  Term.release term