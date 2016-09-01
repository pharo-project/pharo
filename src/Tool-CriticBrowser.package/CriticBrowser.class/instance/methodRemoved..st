system annoucements
methodRemoved: aMethod

	(rbEnvironment includesMethod: aMethod)
		ifTrue: [ cache itemRemoved: aMethod ].
		
	cache updateBrowser