asserting
should: aBlock notTakeMoreThan: aDuration
    "Evaluate aBlock and if it takes more than given duration
    to run we report a test failure. "

	^ aBlock valueWithin: aDuration onTimeout: [
		self 
			assert: false 
			description: ['Block evaluation took more than the expected <1p>' expandMacrosWith: aDuration]
	]
	
