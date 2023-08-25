#!/bin/bash

set -xe

mkdir -p bin
mkdir -p obj

opam switch 5.0.0
eval $(opam env)

ocamlc -o bin/lexer -I obj token.ml lexer.ml
ocamlc -o bin/parser -I obj token.ml parser.ml
ocamlc -o bin/gen -I obj token.ml parser.ml gen.ml
ocamlc -o bin/main -I obj token.ml lexer.ml parser.ml gen.ml main.ml

# mv *.cmo *.cmi obj/

./bin/main
