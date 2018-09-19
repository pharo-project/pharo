test-git-long-describe
testFromLongDescribeWithNumberOfCommitsExtractsSemanticVersion

	| versionInfo |
	versionInfo := PBVersionInfo fromLongGitDescribe: 'v1.5.3-21-g975b'.
	
	self assert: versionInfo major equals: 1.
	self assert: versionInfo minor equals: 5.
	self assert: versionInfo patch equals: 3.
	self assert: versionInfo suffix equals: nil.
	self assert: versionInfo commitHash equals: 'g975b'.