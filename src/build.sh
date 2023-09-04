#!/bin/bash

set -xe

mkdir -p bin
mkdir -p obj

# opam switch 5.0.0
# eval $(opam env)

# $(opam env --switch=5.0.0)

# ocamlc -o bin/lexer -I obj token.ml lexer.ml
# ocamlc -o bin/parser -I obj token.ml lexer.ml parser.ml
# ocamlc -o bin/main -I obj token.ml lexer.ml parser.ml main.ml
# ocamlc -o bin/gen -I obj token.ml parser.ml gen.ml
ocamlc -o bin/main -I obj err.ml ast.ml token.ml lexer.ml parser.ml gen.ml main.ml

if [[ "$1" != "-o" ]]; then
    mv *.cmo *.cmi obj/
fi

./bin/main
