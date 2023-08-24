#!/bin/bash

set -xe

ocamlc -o lexer token.ml lexer.ml
./lexer
