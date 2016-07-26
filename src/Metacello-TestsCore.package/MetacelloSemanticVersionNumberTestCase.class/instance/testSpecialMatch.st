test matching
testSpecialMatch
    self assert: ((self versionClass fromString: '1.1.0') match: '?').
    self assert: ((self versionClass fromString: '1.0.0-alpha') match: '?').
    self assert: ((self versionClass fromString: '1.0.0-rc.1+build.1') match: '?').
    self assert: ((self versionClass fromString: '1.1.0') match: '*.*.?').
    self assert: ((self versionClass fromString: '1.3.7+build.2.b8f12d7') match: '?').
    self assert: ((self versionClass fromString: '1.3.7+build.11.e0f985a') match: '?').
    self deny: ((self versionClass fromString: '1.1.0') match: '*.*.*-?').
    self deny: ((self versionClass fromString: '1.1.0') match: '*.*.*+?').
    self assert: ((self versionClass fromString: '1.1.1') match: '*.?')