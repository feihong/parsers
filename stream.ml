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
  let res = ref [] in
  for _ = 1 to n do
    res := Stream.next stream :: !res
  done;
  List.rev !res

let next_opt stream =
  match Stream.next stream with
  | exception Stream.Failure -> None
  | c -> Some c
