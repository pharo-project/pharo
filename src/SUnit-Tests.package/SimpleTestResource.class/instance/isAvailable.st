testing
isAvailable
	^self class allowAvailability
		and: [self runningState == self startedStateSymbol]