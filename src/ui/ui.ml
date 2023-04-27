(* open Tk open Editor open Text_editor

   let main () = let top = openTk () in Wm.title_set top "OCaml Text Editor";
   let text_widget = Text.create top ~wrap:`None in let load_button =
   Button.create top ~text:"Load" in let save_button = Button.create top
   ~text:"Save" in

   let state = ref (create_editor_state ()) in

   let load_file_callback () = let filename = getOpenFile top in state :=
   load_file filename; Text.delete text_widget (TextIndex (1, 0)) TextIndexEnd;
   List.iter (fun line -> Text.insert text_widget TextIndexEnd (line ^ "\n"))
   (get_text !state) in

   let save_file_callback () = let filename = getSaveFile top in save_file
   !state filename in

   Button.configure load_button ~command:load_file_callback; Button.configure
   save_button ~command:save_file_callback;

   pack [ coe load_button; coe save_button; coe text_widget ] ~side:`Top
   ~fill:`Both ~expand:true; mainLoop ()

   let () = main () *)