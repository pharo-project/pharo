querying
localBranchNames
	self withRepoDo: [ :repo | ^ repo branches collect: #basename ]