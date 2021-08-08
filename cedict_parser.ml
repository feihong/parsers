#use "opal.ml"

type entry = {
  trad : string;
  simp : string;
  pinyin : string list;
  gloss : string list;
}

let exactly_not x = satisfy ((<>) x)
let take_until x = many1 (exactly_not x) >>= fun chars -> exactly x >> return (implode chars)

let pinyin =
  let brackets = between (exactly '[') (exactly ']') in
  let space_sep p = sep_by p (exactly ' ') in
  brackets (space_sep (many1 alpha_num)) => List.map implode

let gloss =
  let slash_sep p = sep_by p (exactly '/') in
  let not_slashes = many1 (exactly_not '/') => implode in
  let* _ = exactly '/' in
  let* items = slash_sep not_slashes in
  let* _ = exactly '/' in
  return items

let entry_parser =
  let* trad = take_until ' ' in
  let* simp = take_until ' ' in
  let* pinyin = pinyin in
  let* _ = exactly ' ' in
  let* gloss = gloss in
  return {trad; simp; pinyin; gloss}
;;

let los = LazyStream.of_string;;

parse entry_parser (los "一丁不識 一丁不识 [yi1 ding1 bu4 shi2] /illiterate/ignorant/");;
