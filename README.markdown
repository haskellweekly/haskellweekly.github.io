# Haskell Weekly

[![Build badge][]][build]

This repository generates the site at [haskellweekly.news][]. Run it locally
with [Stack][]:

1.  `stack --install-ghc build --file-watch --exec 'haskell-weekly rebuild'`

2.  `stack exec haskell-weekly server`

3.  <http://localhost:8000>

[Build badge]: https://travis-ci.org/haskellweekly/haskellweekly.github.io.svg?branch=hakyll
[build]: https://travis-ci.org/haskellweekly/haskellweekly.github.io
[haskellweekly.news]: https://haskellweekly.news
[Stack]: https://docs.haskellstack.org/en/stable/README/
