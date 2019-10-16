:warning:
Not maintained anymore!
Look here instead:
<https://github.com/haskellweekly/haskellweekly>
:warning:

# Haskell Weekly

[![Build badge][]][build]

[Haskell Weekly][] is a free email newsletter about the Haskell programming
language. This repository is responsible for generating Haskell Weekly's
website.

## Contributing content

We appreciate all contributions, from issues to pull requests. Nothing is too
small!

If you want to bring our attention to something, please open an issue! This can
be used for anything from typos to new content. For example, this has been used
for [calls for participation][], [jobs][], and [bugs][].

If you want to make a change yourself, please open a pull request! We encourage
you to make changes when you can, and we'll work with you to get your changes
merged quickly. For instance, this has been used for [packages of the week][],
[featured content][], and [typos][].

### Job postings

We are happy to include job postings for Haskell engineers! If you'd like a job
posting to be included in more than one issue of Haskell Weekly, please consider
advertising with us. This page explains all the details:
<https://haskellweekly.news/advertising.html>.

## Contributing code

The code that powers Haskell Weekly does not change that frequently. However we
still welcome changes to it! The overall guidelines from above also apply to
code. In addition, there are a couple other things to keep in mind:

-   The Haskell Weekly site generator is meant to be run with [Stack][]. You
    may be able to run it with other tools, but they are not officially
    supported.

-   Most small changes can be made without running the generator locally. Every
    pull request will be tested on Travis CI. The tests must pass before the
    pull request will be merged, but we can help you with any problems.

-   If you want or need to run the generator locally, you can do so with this
    command:

    ``` sh
    stack --install-ghc build --file-watch --exec haskell-weekly
    ```

    That will regenerate the site whenever anything changes and put the result
    in the `_site` directory. To actually see the results in your browser,
    you'll need to run a web server. Here is one way to do that:

    ``` sh
    stack exec --package wai-app-static -- warp --docroot _site
    # Open http://localhost:3000 in your browser.
    ```

[Build badge]: https://travis-ci.org/haskellweekly/haskellweekly.github.io.svg?branch=base
[build]: https://travis-ci.org/haskellweekly/haskellweekly.github.io
[Haskell Weekly]: https://haskellweekly.news
[calls for participation]: https://github.com/haskellweekly/haskellweekly.github.io/issues/138
[jobs]: https://github.com/haskellweekly/haskellweekly.github.io/issues/136
[bugs]: https://github.com/haskellweekly/haskellweekly.github.io/issues/62
[packages of the week]: https://github.com/haskellweekly/haskellweekly.github.io/issues/159
[featured content]: https://github.com/haskellweekly/haskellweekly.github.io/issues/148
[typos]: https://github.com/haskellweekly/haskellweekly.github.io/issues/145
[Stack]: https://docs.haskellstack.org/en/stable/README/
