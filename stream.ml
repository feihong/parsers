include Stream

let filter pred stream =
  let rec next i =
    try
      let value = Stream.next stream in
      if pred value then Some value else next i
    with Stream.Failure -> None
  in
  Stream.from next

let of_file_lines in_channel =
  Stream.from (fun _ -> try Some (input_line in_channel) with End_of_file -> None)

let nnext n stream =
  let rec loop n acc =
    match n with
    | 0 -> List.rev acc
    | _ -> loop (n - 1) (Stream.next stream :: acc)
  in
  try Some (loop n []) with Stream.Failure -> None

let next_opt stream = try Some (Stream.next stream) with Stream.Failure -> None
