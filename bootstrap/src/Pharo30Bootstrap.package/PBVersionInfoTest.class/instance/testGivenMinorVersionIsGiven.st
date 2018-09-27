tests
testGivenMinorVersionIsGiven

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	versionInfo minor: 17.
	self assert: versionInfo minor equals: 17.