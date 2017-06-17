instance creation
named: branchName inRepository: repo
	^ self 
		named: branchName
		local: (self isRemoteBranchName: branchName) not
		inRepository: repo