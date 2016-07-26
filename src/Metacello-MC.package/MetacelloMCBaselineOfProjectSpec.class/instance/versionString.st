querying
versionString
    ^ versionString ifNil: [ self version ifNotNil: [:v | v versionString] ]