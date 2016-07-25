tests
testResolveSymbolicVersionsIn
    | initialVersion configurationClass |
    self generateTestVersion.
    configurationClass := Smalltalk globals
        at: #ConfigurationOfVersionnerTestXMLParser.
    initialVersion := configurationClass project version: '3.0'.
    [ cmd resolveSymbolicVersionsIn: initialVersion ]
        on: MetacelloCannotUpdateReleasedVersionError
        do: [ :e |  ].
    self
        assert: (configurationClass methodNamed: #testVersion:) ast
        equals:
            (RBParser parseMethod: self expectedTestResolveSymbolicVersionInResult)