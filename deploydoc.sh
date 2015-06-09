cd doc/html
git add -f --all *
git commit -m "Travis CI autocommit from travis build ${TRAVIS_BUILD_NUMBER}"
git push -fq origin gh-pages
