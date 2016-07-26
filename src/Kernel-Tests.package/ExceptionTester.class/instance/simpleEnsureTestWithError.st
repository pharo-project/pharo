tests
simpleEnsureTestWithError

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		ensure:
			[self doYetAnotherThing].
	