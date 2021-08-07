let file = "cedict.txt" in
let in_channel = open_in file in
try
  while true do
    let line = input_line in_channel in
    if line.[0] <> '#' then (
      print_endline line;
      flush stdout
    )
  done
with End_of_file ->
  close_in in_channel
