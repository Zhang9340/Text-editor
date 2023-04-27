(* open Tk open GMain open GdkKeysyms

   let main () = let _ = GtkMain.Main.init () in let window = GWindow.window
   ~title:"OCaml Text Editor" ~width:800 ~height:600 () in let _ =
   window#connect#destroy ~callback:Main.quit in

   let vbox = GPack.vbox ~packing:window#add () in let menubar = GMenu.menu_bar
   ~packing:vbox#pack () in let factory = new GMenu.factory menubar in let
   accel_group = factory#accel_group in let _ = window#add_accel_group
   accel_group in

   let file_menu = factory#add_submenu "File" in let factory = new GMenu.factory
   file_menu ~accel_group in let _ = factory#add_item "Open" ~key:_O
   ~callback:(fun () -> print_endline "Open") in let _ = factory#add_item "Save"
   ~key:_S ~callback:(fun () -> print_endline "Save") in let _ =
   factory#add_separator () in let _ = factory#add_item "Quit" ~key:_Q
   ~callback:Main.quit in

   let scroll = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
   ~packing:vbox#add () in let buffer = GSourceView2.source_buffer () in let
   view = GSourceView2.source_view ~buffer ~packing:scroll#add_with_viewport ()
   in

   view#misc#modify_font_by_name "Monospace 12";
   buffer#set_highlight_matching_brackets true;

   let _ = window#event#connect#key_press ~callback:(fun ev -> let key =
   GdkEvent.Key.keyval ev in if key = _Q || key = _q then ( Main.quit (); true )
   else false) in

   window#show (); Main.main ()

   let () = main () *)
