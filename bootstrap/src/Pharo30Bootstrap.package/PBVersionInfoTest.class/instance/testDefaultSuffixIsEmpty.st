tests
testDefaultSuffixIsEmpty

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	self assert: versionInfo suffix equals: nil.