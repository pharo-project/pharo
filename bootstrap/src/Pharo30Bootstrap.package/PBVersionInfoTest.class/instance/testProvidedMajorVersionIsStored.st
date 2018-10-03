tests
testProvidedMajorVersionIsStored

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	self assert: versionInfo major equals: 7.