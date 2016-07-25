testing
isMovingToInstVar
	^self isMovingToArgument not 
		and: [(class whoDefinesInstanceVariable: variable) notNil]