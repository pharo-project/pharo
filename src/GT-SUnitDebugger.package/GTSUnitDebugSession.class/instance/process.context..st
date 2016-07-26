initialization
process: aProcess context: aContext

	super process: aProcess context: aContext.

	assertionContext ifNil: [
		self initializeTestContextFrom: aContext.
		self initializeAssestionContextFrom: aContext.
		self supportsDiff ifTrue: [ 
			actualResult := assertionContext tempAt: 1.
			expectedResult := assertionContext tempAt: 2 ]  ]