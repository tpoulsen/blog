#!/bin/bash

set -x

cabal update
cabal install --only-dependencies
