tests
testUnregisterUnknownClassDoesNotFail
	sessionManager registerSystemClassNamed: #Foo.

	sessionManager unregisterClassNamed: #Bar.
	
	self 
		assert: (sessionManager startupList collect: #handledId)
		equals: #( Foo ).