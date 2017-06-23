private
cacheAllBranches
	self remoteBranches do: [ :each | | branchName |
		branchName := each at: 'name'.
		branchesByName 
			at: branchName 
			ifAbsentPut: [ self remoteBranch: branchName ] ]