let file = "cedict.txt"

let line_seq_of_file file =
  let in_channel = open_in file in
  Seq.unfold (fun () ->
    try
      Some (input_line in_channel, ())
    with End_of_file ->
      close_in in_channel;
      None) ()

let () =
  line_seq_of_file file
  |> Seq.filter (fun line -> line.[0] <> '#')
  |> Seq.iter (fun line ->
    print_endline line)
