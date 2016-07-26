tests
testVersion05
    | v1 v2 |
    v1 := self versionClass fromString: '3.0.0'.
    v2 := self versionClass fromString: '2.0.0'.
    self assert: v1 > v2