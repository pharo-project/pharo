tests
testDefaultCommitHashIsUnknown

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	self assert: versionInfo commitHash equals: 'UNKNOWN_COMMIT'.