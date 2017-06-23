querying
currentBranch
	"#currentBranch will look for the branch in the actual repository,
	#branch is the branch that was set to me manually. Normally they should return the same value,
	but if the repository does not yet exist, #currentBranch will fail."

	self withRepoDo: [ :repo |
		^ repo head basename
	]
