#!/usr/bin/env sh
set -o errexit -o xtrace

if ! test -f "$HOME/.local/bin/stack"
then
  URL='https://www.stackage.org/stack/linux-x86_64'
  curl --location "$URL" --output stack.tar.gz
  gunzip stack.tar.gz
  tar --extract --file stack.tar --strip-components 1 --wildcards '*/stack'
  rm stack.tar
  mkdir -p "$HOME/.local/bin"
  mv stack "$HOME/.local/bin/"
fi

stack --version
