(* Lexer for Eva language from http://dmitrysoshnikov.com/courses/essentials-of-interpretation/ *)
#mod_use "stream.ml"

type t =
  | Symbol of string
  | String of string
  | Number of float
  | LParen
  | RParen

let errorf fmt = Printf.ksprintf (fun s -> Error s) fmt

let is_space c = List.mem c ['\n'; '\t'; ' '; '\r']

let implode chars = chars |> List.map (String.make 1) |> String.concat ""

let reverse_implode chars = chars |> List.rev |> implode

let lex_number first_char stream =
  let rec loop ~has_dot ~acc =
    match Stream.peek stream with
    | Some '.' when has_dot -> errorf "Found second dot in number: %s" (reverse_implode ('.' :: acc))
    | Some '.' ->
      Stream.junk stream;
      loop ~has_dot:true ~acc:('.' :: acc)
    | Some ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as c) ->
      Stream.junk stream;
      loop ~has_dot ~acc:(c :: acc)
    | Some _ | None ->
      let s = reverse_implode acc in
      (match float_of_string_opt s with
      | None -> errorf "Invalid number: %s" s
      | Some n -> Ok (Number n))
  in loop ~has_dot:(first_char = '.') ~acc:[first_char]

let lex' stream =
  let rec loop acc =
    match Stream.next stream with
    | exception Stream.Failure -> Ok (List.rev acc)
    | c when is_space c -> loop acc
    | '(' -> loop (LParen :: acc)
    | ')' -> loop (RParen :: acc)
    | '-' | '.' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as c ->
      (match lex_number c stream with
      | Error _ as err -> err
      | Ok token -> loop (token :: acc))
    | c -> errorf "Unexpected character %c" c
  in loop []

let lex s = lex' (Stream.of_string s)

let cases = [
  "1", [Number 1.];
  "-1", [Number (-1.)];
  "801.122", [Number 801.122];
  ".8", [Number 0.8];
  "8.", [Number 8.];
]

let () = cases |> List.iter (fun (s, value) -> assert (lex s = Ok value))
