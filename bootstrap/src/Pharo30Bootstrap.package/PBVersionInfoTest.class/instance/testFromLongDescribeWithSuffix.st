test-git-long-describe
testFromLongDescribeWithSuffix

	| versionInfo |
	versionInfo := PBVersionInfo fromLongGitDescribe: 'v1.5.3-suffix-21-g975b'.
	
	self assert: versionInfo major equals: 1.
	self assert: versionInfo minor equals: 5.
	self assert: versionInfo patch equals: 3.
	self assert: versionInfo suffix equals: 'suffix'.
	self assert: versionInfo commitHash equals: '975b'.