accessing
externalCustomIssue185SHA
  "SHA of commit on 'custom' branch that should be used with this version of tests (https://github.com/dalehenrich/external)"

  "it's too dangerous to use the HEAD of a branch in tests, because over time the HEAD will change to match latest testing needs, but old tests will likely break"

  ^ '29efb3d9cb86da55402a543cbef5771e39f864e7'