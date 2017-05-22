operations
createBranchFromOrigin: branchName
	| remoteBranchName remoteBranch localBranch |
	remoteBranchName := 'origin/', branchName.
	remoteBranch := self lookupRemoteBranch: remoteBranchName.
	localBranch := self createBranch: branchName target: remoteBranch.
	localBranch setUpstream: remoteBranchName.
	^ localBranch