accessing
externalCustomIssue185SHA1
  "SHA of commit on 'custom' branch that should be used with this version of tests (https://github.com/dalehenrich/external)"

  "it's too dangerous to use the HEAD of a branch in tests, because over time the HEAD will change to match latest testing needs, but old tests will likely break"

  ^ '96cf2ed0a252ce55b7fd5ec4bedf3ad999ca6d23'