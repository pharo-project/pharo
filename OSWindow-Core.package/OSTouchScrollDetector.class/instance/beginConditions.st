preconditions
beginConditions
	^ super beginConditions
		and: [ self isKeepingDirection ]