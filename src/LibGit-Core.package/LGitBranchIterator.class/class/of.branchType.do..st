instance creation
of: aRepository branchType: aLGitBranchTypeEnum do: aBlock
	^ (self of: aRepository)
		iterate: aLGitBranchTypeEnum;
		branchesDo: aBlock