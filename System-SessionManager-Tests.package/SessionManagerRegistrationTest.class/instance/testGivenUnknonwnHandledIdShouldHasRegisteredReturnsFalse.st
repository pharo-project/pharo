tests
testGivenUnknonwnHandledIdShouldHasRegisteredReturnsFalse
	self deny: (manager hasRegistered: #Foo)