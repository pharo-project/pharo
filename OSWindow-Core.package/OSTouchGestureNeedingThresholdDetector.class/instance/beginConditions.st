preconditions
beginConditions
	^ super beginConditions and: [ self eachFingersHadTravelledEnough ]