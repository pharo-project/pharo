accessing
externalCustomIssue181SHA
  "SHA of commit on 'custom' branch that should be used with this version of tests (https://github.com/dalehenrich/external)"

  "it's too dangerous to use the HEAD of a branch in tests, because over time the HEAD will change to match latest testing needs, but old tests will likely break"

  ^ '36ac04abfc3a173de9432a134a718bf14b0a6968'