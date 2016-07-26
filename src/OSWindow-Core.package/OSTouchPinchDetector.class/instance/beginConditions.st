preconditions
beginConditions
	^ super beginConditions
		and: [ self areFingersLinesNotTooFarApart
		and: [ self doesFingersGoesInOppositeDirection ]]