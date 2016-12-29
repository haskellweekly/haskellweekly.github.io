#!/usr/bin/env sh
set -o errexit -o xtrace

test "$TRAVIS_BRANCH" = 'hakyll'
test "$TRAVIS_PULL_REQUEST" = 'false'

cd _hakyll/site
echo haskellweekly.news > CNAME
git init
git add .
git config --global user.email 'taylor@fausak.me'
git config --global user.name 'Taylor Fausak'
git commit --author 'Haskell Weekly <info@haskellweekly.news>' --message "Automatic deploy of $TRAVIS_COMMIT"

set -o verbose +o xtrace
git remote add origin "https://$GITHUB_TOKEN@github.com/haskellweekly/haskellweekly.github.io.git"
set +o verbose -o xtrace

git push --force --quiet origin master
