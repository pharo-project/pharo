asserting
assertMissing: aVersionInfo
	self assert: (repository versionWithInfo: aVersionInfo) isNil