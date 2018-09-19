tests
testGivenCommitHashIsGiven

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	versionInfo commitHash: 'abcabcabcabcabcabc'.
	self assert: versionInfo commitHash equals: 'abcabcabcabcabcabc'.