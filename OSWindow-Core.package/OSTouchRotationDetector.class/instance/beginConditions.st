preconditions
beginConditions
	^ super beginConditions and: [ self isBrinkAngleOverwhelmed ]