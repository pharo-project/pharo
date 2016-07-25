test matching
testStandardMatch

	self assert: ((self versionClass fromString: '1.1.1') match: '*.*.*').
	self deny: ((self versionClass fromString: '1.1.1') match: '*.*').
	self assert: ((self versionClass fromString: '1.1.0') match: '1.1.*').
	self assert: ((self versionClass fromString: '1.1.0') match: '1.#.*').
	self deny: ((self versionClass fromString: '1.10.0') match: '1.#.*').
	self assert: ((self versionClass fromString: '1-alpha5.0') match: '1-alpha#.0').
	self assert: ((self versionClass fromString: '1.alpha5.0') match: '1-alpha#.0').
	self assert: ((self versionClass fromString: '1.alpha5.0') match: '1.alpha#.0').
	self deny: ((self versionClass fromString: '1-alpha10.0') match: '1-alpha#.0').
