tests
testEqualityComparison
    self deny: '1.0.0+-' asMetacelloSemanticVersionNumber = '1.0.0--' asMetacelloSemanticVersionNumber.
    self sampleVersionStrings
        do: [ :versionString | self assert: versionString asMetacelloSemanticVersionNumber = versionString asMetacelloSemanticVersionNumber ]