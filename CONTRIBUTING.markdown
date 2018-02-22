# Contributing

Thanks for contributing to Haskell Weekly! We appreciate all contributions,
from issues to pull requests. Nothing is too small! By participating with this
project, you agree to abide by the [code of conduct][].

If you want to bring our attention to something, please open an issue! This can
be used for anything from typos to new content. For example, this has been used
for events (#119), calls for participation (#138), jobs (#136), and bugs (#62).

If you want to make a change yourself, please open a pull request! We encourage
you to make changes when you can, and we'll work with you to get your changes
merged quickly. For instance, this has been used for events (#160), packages of
the week (#159), featured content (#148), and typos (#145).

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

-   You can run the generator locally with this command:

    ``` sh
    stack --install-ghc build --file-watch --exec haskell-weekly
    ```

    That will regenerate the site whenever anything changes and put the result
    in the `_site` directory. To actually see the results in your browser,
    you'll need to run a web server. Here is one way to do that:

    ``` sh
    stack build wai-app-static
    stack exec -- warp --docroot _site
    # Open http://localhost:8080 in your browser.
    ```

[code of conduct]: ./CODE_OF_CONDUCT.markdown
[Stack]: https://docs.haskellstack.org/en/stable/README/
