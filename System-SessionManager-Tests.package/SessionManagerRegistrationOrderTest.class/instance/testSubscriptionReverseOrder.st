tests
testSubscriptionReverseOrder
	manager
		registerSystemClassNamed: 'aFirstSubscription' atPriority: 100;
		register: (ClassSessionHandler forClassNamed: 'anObject').

	self 
		assertHandledId: manager startupList 
		equals: #( 'aFirstSubscription' 'anObject' ).
	self 
		assertHandledId: manager shutdownList 
		equals: #( 'anObject' 'aFirstSubscription' ).