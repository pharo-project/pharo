instance creation
remoteBranchesOf: repository do: aBlock
	self 
		of: repository 
		branchType: LGitBranchTypeEnum git_branch_remote
		do: aBlock