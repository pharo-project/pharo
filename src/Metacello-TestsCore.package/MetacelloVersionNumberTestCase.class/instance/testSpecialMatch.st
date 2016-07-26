test matching
testSpecialMatch

	self assert: ((self versionClass fromString: '1.1') match: '?').
	self assert: ((self versionClass fromString: 'alpha') match: '?').
	self assert: ((self versionClass fromString: '1.1.1.1.1') match: '?').
	self assert: ((self versionClass fromString: '1.alpha5') match: '?').
	self assert: ((self versionClass fromString: '1.1') match: '*.?').
	self assert: ((self versionClass fromString: '1.1') match: '?').
	self assert: ((self versionClass fromString: '10.1.1.1.1') match: '?').
	self deny: ((self versionClass fromString: '1.1') match: '*.*.?').
	self assert: ((self versionClass fromString: '1.1.1') match: '*.?').
