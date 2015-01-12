#!/bin/sh

cabal install --enable-tests --only-dependencies

cabal build
