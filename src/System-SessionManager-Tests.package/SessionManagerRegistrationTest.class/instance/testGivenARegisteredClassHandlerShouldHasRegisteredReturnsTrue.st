tests
testGivenARegisteredClassHandlerShouldHasRegisteredReturnsTrue
	manager registerSystemClassNamed: #Foo.

	self assert: (manager hasRegistered: #Foo)