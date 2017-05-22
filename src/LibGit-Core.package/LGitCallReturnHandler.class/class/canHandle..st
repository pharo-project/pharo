testing
canHandle: aReturnCode
	^ self handledReturnCodes anySatisfy: [ :symbol | 
		aReturnCode symbol = symbol ]