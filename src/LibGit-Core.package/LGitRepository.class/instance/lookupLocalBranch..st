lookup
lookupLocalBranch: name
	^ self lookupBranch: name type: LGitBranchTypeEnum git_branch_local 