tests
testGivenManyRegistrationsInSameCategoryAtDifferentPriorityShoudlAClassBePresentOnlyOnceInStartupList
	manager registerSystemClassNamed: #Foo.
	manager registerSystemClassNamed: #Bar atPriority: 15.
	manager registerSystemClassNamed: #Foo atPriority: 10.
	manager registerSystemClassNamed: #Foo atPriority: 20.
	
	self 
		assert: (manager startupList collect: #handledId)
		equals: #( Bar Foo ).