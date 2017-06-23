private
commitsInBranch: branchName do: aBlock
	"Iterates commit in a branch, in reverse chronological order (latest first)"
	self withRepoDo: [ :repo |
		(LGitRevwalk of: repo)
			pushReference: (repo lookupBranch: branchName);
			beSortedReverse;
			do: aBlock.
	]
