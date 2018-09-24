test-default-values
testDefaultBuildIsUnknown

	| versionInfo |
	versionInfo := PBVersionInfo major: 7.
	self assert: versionInfo buildNumber equals: -1