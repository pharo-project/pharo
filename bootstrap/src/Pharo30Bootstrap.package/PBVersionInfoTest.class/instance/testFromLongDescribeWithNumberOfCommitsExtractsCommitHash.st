test-git-long-describe
testFromLongDescribeWithNumberOfCommitsExtractsCommitHash

	| versionInfo |
	versionInfo := PBVersionInfo fromLongGitDescribe: 'v1.5.3-21-g975b'.	
	self assert: versionInfo commitHash equals: '975b'.