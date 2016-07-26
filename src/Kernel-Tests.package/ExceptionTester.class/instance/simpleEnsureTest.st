tests
simpleEnsureTest

	[self doSomething.
	self doSomethingElse]
		ensure:
			[self doYetAnotherThing].
	