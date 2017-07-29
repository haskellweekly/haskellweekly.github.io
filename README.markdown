# Haskell Weekly

[![Build badge][]][build]

This repository generates the site at [haskellweekly.news][].

You can run this site locally with [Stack][]:

1.  Install GHC with `stack setup`.

2.  Build the project with `stack build --file-watch --exec haskell-weekly`.

3.  Build the server with `stack build wai-app-static`.

4.  Run the server with `stack exec warp -- --docroot _site`.

5.  Go to <http://localhost:3000>.

[Build badge]: https://travis-ci.org/haskellweekly/haskellweekly.github.io.svg?branch=hakyll
[build]: https://travis-ci.org/haskellweekly/haskellweekly.github.io
[haskellweekly.news]: https://haskellweekly.news
[Stack]: https://docs.haskellstack.org/en/stable/README/
