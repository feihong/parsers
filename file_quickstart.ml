let file = "cedict.txt"

let line_stream_of_file file =
  let in_channel = open_in file in
  Stream.from (fun _ ->
    try
      Some (input_line in_channel)
    with End_of_file ->
      close_in in_channel;
      None)

let () =
  line_stream_of_file file
  |> Stream.iter (fun line ->
    print_endline line)
