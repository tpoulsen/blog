#!/bin/bash
set -x
if [[ !$TRAVIS_PULL_REQUEST && $TRAVIS_BRANCH == 'master' ]] ; then
  # initalize new repo in _site and push to server
  cd _site
  git init

  git remote add deploy "deploy@travispoulsen.com:/var/www/travispoulsen.com"
  git config user.name "Travis CI"
  git config user.email "travis.poulsen+travisci@gmail.com"

  git add .
  git commit -m "Deploy"
  git push --force deploy master
else
  echo "Not master, not deploying."
fi
