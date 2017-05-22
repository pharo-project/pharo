actions
createBranch: branchName
	self withRepoDo: [ :repo |
		| newBranch |
		newBranch := repo createBranch: branchName target: repo head.
		repo setHead: newBranch
	].
	self branch: self lookupHead.
