#!/usr/bin/env stack
{-
  stack
  --resolver nightly-2017-11-25
  script
  --package directory
  --package filepath
  --package process
-}

import qualified Control.Monad as Monad
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import qualified System.Process as Process

main :: IO ()
main = do
  token <- Environment.getEnv "GITHUB_TOKEN"
  branch <- Environment.getEnv "TRAVIS_BRANCH"
  commit <- Environment.getEnv "TRAVIS_COMMIT"
  isPullRequest <- Environment.getEnv "TRAVIS_PULL_REQUEST"

  Monad.guard (branch == "hakyll")
  Monad.guard (isPullRequest == "false")

  Directory.setCurrentDirectory (FilePath.joinPath ["_site"])
  writeFile "CNAME" "haskellweekly.news"
  let git = Process.callProcess "git"

  git ["init"]
  git ["add", "."]
  git ["config", "--global", "user.email", "taylor@fausak.me"]
  git ["config", "--global", "user.name", "Taylor Fausak"]
  git ["commit", "--author", "Haskell Weekly <info@haskellweekly.news>", "--message", "Automatic deploy of " ++ commit]
  git ["remote", "add", "origin", "https://" ++ token ++ "@github.com/haskellweekly/haskellweekly.github.io.git"]
  git ["push", "--force", "--quiet", "origin", "master"]
