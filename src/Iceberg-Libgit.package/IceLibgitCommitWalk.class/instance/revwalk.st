accessing
revwalk
	^ revwalk ifNil: [
		repository withRepoDo: [ :repo |
			"Keep a reference to the repo so that it does not get garbage collected 
			(that would free C memory and make leave the revwalk object dumb)"

			lgitRepository := repo. 
			^ revwalk := LGitRevwalk of: repo.
		]
 	]
