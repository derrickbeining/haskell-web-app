#! /usr/bin/env sh

ghcid \
  --warnings \
  -c 'stack repl' \
  --reload=./app/Main.hs \
  --reload=./src \
  -T Main.main \
  --restart=./package.yaml \
  --restart=./stack.yaml \
  --restart=./web-haskell.cabal
