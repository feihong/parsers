(* open Opal *)

type entry = {
  trad : string;
  simp : string;
  pinyin : string;
  gloss : string list;
}

let exactly_not x = satisfy ((<>) x)
let not_space = exactly_not ' '
let slash_sep p = sep_by p (exactly '/')
let not_slashes = many1 (exactly_not '/') => implode

let entry_parser =
  let* trad = many1 not_space => implode in
  let* _ = space in
  let* simp = many1 not_space => implode in
  let* _ = token "[" in
  let* pinyin = many1 (exactly_not ']') => implode in
  let* _ = token "] /" in
  let* gloss = slash_sep not_slashes in
  let* _ = exactly '/' in
  return {trad; simp; pinyin; gloss}

(* parse entry_parser (LazyStream.of_string "一丁不識 一丁不识 [yi1 ding1 bu4 shi2] /illiterate/ignorant/");; *)
