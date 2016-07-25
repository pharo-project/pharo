tests
testGivenManyRegistrationsInSameCategoryShoudlAClassBePresentOnlyOnceInStartupList
	manager registerSystemClassNamed: #Foo.
	manager registerSystemClassNamed: #Bar.
	manager registerSystemClassNamed: #Foo.
	manager registerSystemClassNamed: #Foo.
	
	self 
		assert: (manager startupList collect: #handledId)
		equals: #( Bar Foo ).