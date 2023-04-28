(* let _ = Ui.test *)
(* open Notty *)
open Notty_unix

(* let square = "\xe2\x96\xaa" *)
let example = [ "a ad "; "qw era sdf"; "caf d" ]
let image = Ui.convert example
(* let rec sierp n = if n > 1 then let ss = sierp (pred n) in I.(ss <-> (ss <|>
   ss)) else I.(string A.(fg magenta) square |> hpad 1 0) *)

(* let img (double, n) = let s = sierp n in if double then I.(s </> vpad 1 0 s)
   else s *)

let rec update t img =
  Term.image t img;
  loop t img

and loop t img =
  match Term.event t with
  | `Key (`Enter, _) -> ()
  | _ -> loop t img

let t = Term.create ()

let _ =
  update t image;
  Term.release t
