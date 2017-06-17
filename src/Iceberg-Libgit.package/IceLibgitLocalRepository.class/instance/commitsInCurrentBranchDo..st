private
commitsInCurrentBranchDo: aBlock
	self withRepoDo: [ :repo |
		(LGitRevwalk of: repo)
			pushHead;
			do: aBlock.
	]
