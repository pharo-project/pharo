tests
testGivenBuildIsGiven

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	versionInfo buildNumber: 200.
	self assert: versionInfo buildNumber equals: 200.