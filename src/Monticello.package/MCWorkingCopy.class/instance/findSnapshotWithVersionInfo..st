private
findSnapshotWithVersionInfo: aVersionInfo
    "when an ancestor inside the ancestor chain is not found, does not pass nil instead.
    With this change we can now browse history and delta between them without having to 
    have the complete history"
    "instead of asking for the user to add a new repository, or copy the the missing package we simply
    return an empty Snapshot by returning nil"

    ^ aVersionInfo
        ifNil: [MCSnapshot empty]
        ifNotNil: [(self repositoryGroup versionWithInfo: aVersionInfo ifNone: [nil])
            ifNil: [MCSnapshot empty]
            ifNotNil: [:aVersion | aVersion snapshot]]