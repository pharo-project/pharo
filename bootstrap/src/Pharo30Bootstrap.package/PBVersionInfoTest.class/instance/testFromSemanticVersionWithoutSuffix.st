test-semantic-version
testFromSemanticVersionWithoutSuffix

	| versionInfo |
	versionInfo := PBVersionInfo fromSemanticVersion: 'v1.2.3'.
	
	self assert: versionInfo major equals: 1.
	self assert: versionInfo minor equals: 2.
	self assert: versionInfo patch equals: 3.
	self assert: versionInfo suffix equals: nil.