instance creation
allBranchesOf: repository do: aBlock
	self 
		of: repository 
		branchType: LGitBranchTypeEnum git_branch_all
		do: aBlock