tests
testGivenManyRegistrationsInDifferentCategoriesShoudlLastClassRegistrationBeTheOnlyOccurenceInStartupList
	manager registerSystemClassNamed: #Foo.
	manager registerSystemClassNamed: #Bar.
	manager registerGuiClassNamed: #Foo atPriority: 30.
	manager registerGuiClassNamed: #Foo atPriority: 50.
	
	self 
		assert: (manager startupList collect: #handledId)
		equals: #( Bar Foo ).