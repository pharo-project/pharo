private auxiliar
withRepoDo: aBlock
	LGitGlobal runSequence: [ 
		| repo |
		repo := (LGitRepository on: self location) open.
		aBlock value: repo ]
