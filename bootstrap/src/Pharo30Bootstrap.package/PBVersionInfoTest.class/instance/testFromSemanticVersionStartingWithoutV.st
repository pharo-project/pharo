test-semantic-version
testFromSemanticVersionStartingWithoutV

	| versionInfo |
	versionInfo := PBVersionInfo fromSemanticVersion: '1.2.3-alpha'.
	
	self assert: versionInfo major equals: 1.
	self assert: versionInfo minor equals: 2.
	self assert: versionInfo patch equals: 3.
	self assert: versionInfo suffix equals: 'alpha'.