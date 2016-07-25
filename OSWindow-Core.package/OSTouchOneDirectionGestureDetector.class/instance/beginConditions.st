preconditions
beginConditions
	^ super beginConditions and: [ self allFingersGoesInTheSameDirection ]