let read_file filename =
  let file = open_in filename in
  try
    while true do
      let line = input_line file in
      print_endline line
    done
  with End_of_file ->
    close_in file