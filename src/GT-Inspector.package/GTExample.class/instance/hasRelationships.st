testing
hasRelationships
	^ self hasDependents
		or: [ self hasDependencies ]