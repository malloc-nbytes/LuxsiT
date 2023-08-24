#!/bin/bash

set -xe

ocamlc -o parser token.ml parser.ml
./parser
