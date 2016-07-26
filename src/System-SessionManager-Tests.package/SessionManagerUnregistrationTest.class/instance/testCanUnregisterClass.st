tests
testCanUnregisterClass
	sessionManager registerSystemClassNamed: #Foo.
	sessionManager registerSystemClassNamed: #Bar.

	sessionManager unregisterClassNamed: #Foo.
	
	self 
		assert: (sessionManager startupList collect: #handledId)
		equals: #( Bar ).