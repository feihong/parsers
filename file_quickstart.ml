#mod_use "stream.ml"

let stream_take (n : int) stream =
  let rec next i =
    try
      if i >= n then None else Some (Stream.next stream)
    with Stream.Failure -> None
  in
  Stream.from next

let () =
  let in_channel = open_in "cedict.txt" in
  Stream.of_file_lines in_channel
  |> Stream.filter (fun line -> line.[0] <> '#')
  |> stream_take 100
  |> Stream.iter (fun line -> print_endline line);
  close_in in_channel
