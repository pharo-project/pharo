accessing
externalCustomIssue185SHA2
  "SHA of commit on 'custom' branch that should be used with this version of tests (https://github.com/dalehenrich/external)"

  "it's too dangerous to use the HEAD of a branch in tests, because over time the HEAD will change to match latest testing needs, but old tests will likely break"

  ^ '9e79d0e165bd51f7476d10a07968706f6034a6e6'