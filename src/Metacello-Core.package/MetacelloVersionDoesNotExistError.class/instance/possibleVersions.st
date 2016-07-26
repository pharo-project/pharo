accessing
possibleVersions
    ^ self project symbolicVersionSymbols , (self project versions collect: [ :each | each versionString ])