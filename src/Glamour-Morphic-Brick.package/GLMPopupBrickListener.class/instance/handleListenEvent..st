events-processing
handleListenEvent: anEvent

	self predicates
		detect: [ :aPredicate | aPredicate popup: self popup event: anEvent ]
		ifFound: [ :aPredicate | self act ]