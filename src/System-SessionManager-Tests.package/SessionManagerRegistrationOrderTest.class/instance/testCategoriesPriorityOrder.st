tests
testCategoriesPriorityOrder
	manager 
		register: (ClassSessionHandler forClassNamed: 'anObject');
		registerSystemClassNamed: 'aFirstSubscription' atPriority: 100.
	
	self 
		assertHandledId: manager startupList 
		equals: #( 'aFirstSubscription' 'anObject' ).