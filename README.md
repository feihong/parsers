# Parsers

## Prerequisites

    brew install opam
    opam init
    eval $(opam env)
    opam switch create 4.12.0
    opam install utop menhir

## Quickstart

Use `opal` inside of `utop`:

    utop
    utop # #use "opal.ml";;
    utop # let input = LazyStream.of_string "foo is great" in
    let foo = token "foo" in
    parse foo input;;
    - : string option = None

Run `cedict_parser.ml` as script:

    ocaml cedict_parser.ml

Run `cedict_parser.ml` inside of utop:

    utop
    utop # #use "cedict_parser.ml";;

## Links

- [Opal: Monadic Parser Combinators for OCaml](https://github.com/pyrocat101/opal)
- [Angstrom: Parser combinators built for speed and memory-efficiency](https://github.com/inhabitedtype/angstrom)
- [OCaml Tutorials: Streams](https://ocaml.org/learn/tutorials/streams.html)
- [The toplevel system or REPL](https://ocaml.org/manual/toplevel.html)
- [Functional Programming in OCaml: Interpreters](https://www.cs.cornell.edu/courses/cs3110/2021sp/textbook/interp/intro.html)
