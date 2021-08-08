# Parsers

## Prerequisites

    brew install opam
    opam init
    eval $(opam env)
    opam switch create 4.12.0
    opam install utop

## Quickstart

    utop
    utop # #use "opal.ml";;
    utop # let input = LazyStream.of_string "foo is great" in
    let foo = token "foo" in
    parse foo input;;
    - : string option = None

## Links

- [Opal: Monadic Parser Combinators for OCaml](https://github.com/pyrocat101/opal)
- [OCaml Tutorials: Streams](https://ocaml.org/learn/tutorials/streams.html)
- [The toplevel system or REPL](https://ocaml.org/manual/toplevel.html)
