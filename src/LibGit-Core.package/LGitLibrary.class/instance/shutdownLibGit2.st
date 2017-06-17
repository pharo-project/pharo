operations
shutdownLibGit2

	| initsRemaining |
	
	initsRemaining := self libgit2_shutdown.
	initsRemaining <= 0
		ifTrue: [ Initialized := false ].
	^ initsRemaining