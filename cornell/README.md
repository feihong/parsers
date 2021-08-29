# Lexer and parser for SimPL

Adapted from https://www.cs.cornell.edu/courses/cs3110/2021sp/textbook/interp/simpl_frontend.html

Migrated from ocamlbuild to dune, and from ocamllex to sedlex.

## Notes

In `dune`, don't put `sedlex_ppx` in `libraries` as in the [sedlex examples dune](https://github.com/ocaml-community/sedlex/blob/master/examples/dune). It will fail to find it. Instead, put substitute it for `ppxlib`.

## Links

- https://schu.be/til-sedlex-and-menhir.html
- https://github.com/jorisgio/menhir-workshop/
- https://github.com/sufrin/InteractiveSedlexMenhirExample
- https://github.com/ocaml-community/sedlex/tree/master/examples
