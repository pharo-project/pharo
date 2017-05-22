lookup
lookupRemoteBranch: name
	^ self lookupBranch: name type: LGitBranchTypeEnum git_branch_remote