accessing
externalCustomIssue185SHA3
  "SHA of commit on 'custom' branch that should be used with this version of tests (https://github.com/dalehenrich/external)"

  "it's too dangerous to use the HEAD of a branch in tests, because over time the HEAD will change to match latest testing needs, but old tests will likely break"

  ^ '11dfa72bc4a956ce0cb5a7603ea39f5abf73ff1c'