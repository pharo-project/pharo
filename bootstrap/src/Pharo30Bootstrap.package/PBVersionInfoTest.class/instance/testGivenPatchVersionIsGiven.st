tests
testGivenPatchVersionIsGiven

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	versionInfo patch: 88.
	self assert: versionInfo patch equals: 88.