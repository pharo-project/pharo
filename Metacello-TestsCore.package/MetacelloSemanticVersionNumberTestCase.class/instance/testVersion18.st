tests
testVersion18
    | x y |
    self deny: (x := self versionClass fromString: '1.0.0') < (y := self versionClass fromString: '1.0.0-0').
    self assert: (x := self versionClass fromString: '1.0.0') > (y := self versionClass fromString: '1.0.0-0').
    self assert: (x := self versionClass fromString: '1.0.0') < (y := self versionClass fromString: '1.0.0+0').
    self deny: (x := self versionClass fromString: '1.0.0') > (y := self versionClass fromString: '1.0.0+0')