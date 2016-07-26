tests
testRegisterElementByDefault
	manager register: (ClassSessionHandler forClassNamed: 'anObject').

	self 
		assertHandledId: manager startupList
		equals: #( 'anObject' ).