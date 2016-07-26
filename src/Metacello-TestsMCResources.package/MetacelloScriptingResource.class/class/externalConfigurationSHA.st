accessing
externalConfigurationSHA
    "SHA of commit on 'configuration' branch that should be used with this version of tests (https://github.com/dalehenrich/external)"

    "it's too dangerous to use the HEAD of a branch in tests, because over time the HEAD will change to match latest testing needs, but old tests will likely break"

    "self reset"

    ^ 'd91949731736bf48879781c29c7365feca461cde'