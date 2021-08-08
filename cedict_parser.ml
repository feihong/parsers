#use "opal.ml"
#mod_use "stream.ml"

type entry = {
  trad : string;
  simp : string;
  pinyin : string list;
  gloss : string list;
}

let exactly_not x = satisfy ((<>) x)
let take_until x = many1 (exactly_not x) >>= fun chars -> exactly x >> return (implode chars)
let exactly_s s =
  let rec loop s i =
    if i >= String.length s
    then return s
    else exactly s.[i] >> loop s (i + 1)
  in
  loop s 0

let pinyin =
  let brackets = between (exactly '[') (exactly ']') in
  let space_sep p = sep_by p (exactly ' ') in
  let comma = exactly_s "," in
  let dot = exactly_s "·" in  (* unicode dot *)
  let morpheme = many1 (alpha_num <|> (exactly ':')) => implode in
  let item = comma <|> dot <|> morpheme in
  brackets (space_sep item)

let gloss =
  let slash_sep p = sep_by p (exactly '/') in
  let not_slashes = many1 (exactly_not '/') => implode in
  let* _ = exactly '/' in
  let* items = slash_sep not_slashes in
  let* _ = exactly '/' in
  return items

let entry =
  let* trad = take_until ' ' in
  let* simp = take_until ' ' in
  let* pinyin = pinyin in
  let* _ = exactly ' ' in
  let* gloss = gloss in
  return {trad; simp; pinyin; gloss}


let parse_string p s = parse p (LazyStream.of_string s)

let process_line line =
  let line = String.trim line in
  match parse_string entry line with
  | None ->
    Printf.printf "error: %s\n" line;
    raise Stream.Failure
  | Some {trad; simp; pinyin; _} ->
    Printf.printf "trad: %s, simp: %s, pinyin: %s\n" trad simp (String.concat " " pinyin)

let () =
  let in_channel = open_in "cedict.txt" in
  Stream.of_file_lines in_channel
  |> Stream.filter (fun line -> line.[0] <> '#')
  |> Stream.iter process_line;
  print_endline "Parsed everything!";
  close_in in_channel

(* parse_string entry "一丁不識 一丁不识 [yi1 ding1 bu4 shi2] /illiterate/ignorant/");; *)
