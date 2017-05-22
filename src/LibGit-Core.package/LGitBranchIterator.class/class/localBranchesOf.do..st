instance creation
localBranchesOf: repository do: aBlock
	self 
		of: repository 
		branchType: LGitBranchTypeEnum git_branch_local
		do: aBlock