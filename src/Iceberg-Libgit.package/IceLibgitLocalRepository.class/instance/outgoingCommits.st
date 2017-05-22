querying
outgoingCommits

	self withRepoDo: [ :repo |
		| currentBranch commits walk |
		
		commits := OrderedCollection new.
		currentBranch := repo lookupBranch: self branch name.
	
		walk := (LGitRevwalk of: repo)
			pushReference: currentBranch;
			hideReference: currentBranch upstream
			yourself.
			
		[ walk hasNext ] whileTrue: [ 
			commits add: walk next
		].

		^ commits
	]
