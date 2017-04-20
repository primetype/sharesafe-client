#! /bin/sh

set -xe

stack build

stack exec -- sharesafe-client-purescript-bridge ${PWD}/ui/frontend
