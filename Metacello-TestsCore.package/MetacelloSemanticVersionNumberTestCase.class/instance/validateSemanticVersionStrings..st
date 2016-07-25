tests
validateSemanticVersionStrings: versionStrings
    | versions version |
    versions := versionStrings collect: [ :each | each asMetacelloSemanticVersionNumber ].
    version := versions at: 1.
    2 to: versions size do: [ :index | 
        | nextVersion |
        nextVersion := versions at: index.
        self assert: version < nextVersion.
        version := nextVersion ]