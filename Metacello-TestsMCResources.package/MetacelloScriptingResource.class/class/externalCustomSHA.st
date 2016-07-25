accessing
externalCustomSHA
    "SHA of commit on 'custom' branch that should be used with this version of tests (https://github.com/dalehenrich/external)"

    "it's too dangerous to use the HEAD of a branch in tests, because over time the HEAD will change to match latest testing needs, but old tests will likely break"

    "self reset"

    ^ '1ac58502ade7814e1590f71d615cca434b1a4fd5'