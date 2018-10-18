#!/usr/bin/env sh
set -o errexit -o xtrace

if ! test -f "$HOME/.local/bin/stack"
then
  VERSION=1.9.1
  URL="https://github.com/commercialhaskell/stack/releases/download/v$VERSION/stack-$VERSION-linux-x86_64.tar.gz"
  curl --location "$URL" --output stack.tar.gz
  gunzip stack.tar.gz
  tar --extract --file stack.tar --strip-components 1 --wildcards '*/stack'
  rm stack.tar
  mkdir -p "$HOME/.local/bin"
  mv stack "$HOME/.local/bin/"
fi

stack --version
