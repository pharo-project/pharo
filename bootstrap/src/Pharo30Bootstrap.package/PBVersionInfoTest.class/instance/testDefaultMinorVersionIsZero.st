tests
testDefaultMinorVersionIsZero

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	self assert: versionInfo minor equals: 0.