tests
testGivenARegisteredClassRenamedThenItsHandlerIsUpdated
	manager registerSystemClassNamed: #Foo.
	manager renamedClass: self from: #Foo to: #Bar.
	
	self 
		assert: (manager startupList collect: #handledId)
		equals: #( Bar ).