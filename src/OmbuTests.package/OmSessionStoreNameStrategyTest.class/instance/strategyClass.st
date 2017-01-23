initialization
strategyClass

	^ self class environment classNamed: 
		(self class name allButLast: 4) "corresponding to the word Test"