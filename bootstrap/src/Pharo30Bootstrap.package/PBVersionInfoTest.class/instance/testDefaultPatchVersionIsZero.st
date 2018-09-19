tests
testDefaultPatchVersionIsZero

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	self assert: versionInfo patch equals: 0.