computing
possibleDeadCode
	^ self missingMethodsWithPotentialMatch
		select: #isEmpty