#use "opal.ml"
#mod_use "stream.ml"

type entry = {
  chapterTitle : string;
  time : string;
  text : string;
  annotation : string;
}

type notes = {
  title : string;
  author : string;
  entries : entry list;
}

let exactly_not x = satisfy ((<>) x)
let take_until x = many (exactly_not x) >>= fun chars -> exactly x >> return (implode chars)
let exactly_s s =
  let rec loop s i =
    if i >= String.length s
    then return s
    else exactly s.[i] >> loop s (i + 1)
  in
  loop s 0

let title =
  let* _ = exactly_s "BOOX Reading Notes | <<" in
   (* could fail for titles with '>' in them, but not sure how to properly handle it *)
  let* title = take_until '>' in
  let* _ = exactly_s ">\n" in
  return title

let entry =
  let* chapterTitle = take_until '\n' in
  let* _ = exactly_s "Time：" in
  let* time = take_until '\n' in
  let* _ = exactly_s "【Original Text】" in
  let* text = take_until '\n' in
  let* _ = exactly_s "【Annotations】" in
  let* annotation = take_until '\n' in
  return {chapterTitle; time; text; annotation}

let divider = many1 (exactly '-') >>= fun _ -> newline >> return ()

let notes =
  let* title = title in
  let* author = take_until '\n' in
  let* entries = sep_by entry divider in
  (* let* entry = entry in *)
  return {title; author; entries }

let () =
  let ic = open_in "sample_notes.txt" in
  match parse notes (LazyStream.of_channel ic) with
  | None -> ()
  | Some {title; author; entries} ->
    Printf.printf "title: %s, author: %s\n\n" title author;
    entries |> List.iter (fun {chapterTitle; text; annotation; _} ->
      Printf.printf "title: %s; text: %s; annotation: %s\n" chapterTitle text annotation);
  close_in ic

(*

let parse_string p s = parse p (LazyStream.of_string s)

let s =
  let read_file f =
    let ic = open_in f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    (Bytes.unsafe_to_string s)
  in
  read_file "sample_notes.txt"

parse_string notes s;;
*)
