tests
testRegisterSystemElement
	manager registerSystemClassNamed: 'anObject' atPriority: 1.

	self 
		assertHandledId: manager startupList
		equals: #( 'anObject' ).