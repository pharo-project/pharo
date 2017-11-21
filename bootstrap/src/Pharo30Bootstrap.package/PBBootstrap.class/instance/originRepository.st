accessing
originRepository

	| repositoryReference |
	
	repositoryReference := (Smalltalk os environment at: 'BOOTSTRAP_REPOSITORY' ifAbsent: ['.']) asFileReference / 'src'.


	^ TonelRepository new directory: repositoryReference