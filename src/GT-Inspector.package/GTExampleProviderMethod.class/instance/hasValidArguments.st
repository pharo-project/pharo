testing
hasValidArguments
	^ self exists 
		and: [ self arguments size = self method numArgs ]