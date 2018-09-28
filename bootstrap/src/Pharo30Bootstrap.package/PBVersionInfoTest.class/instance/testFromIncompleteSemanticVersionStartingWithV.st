test-semantic-version
testFromIncompleteSemanticVersionStartingWithV

	| versionInfo |
	versionInfo := PBVersionInfo fromSemanticVersion: 'v70-alpha'.
	
	self assert: versionInfo major equals: 70.
	self assert: versionInfo minor equals: 0.
	self assert: versionInfo patch equals: 0.
	self assert: versionInfo suffix equals: 'alpha'.