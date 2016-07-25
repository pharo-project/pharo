preconditions
checkForPrimitiveMethod
	parseTree isPrimitive 
		ifTrue: [self refactoringError: 'Cannot move primitive methods']