tests
testGivenSuffixIsGiven

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	versionInfo suffix: 'SNAPSHOT'.
	self assert: versionInfo suffix equals: 'SNAPSHOT'.