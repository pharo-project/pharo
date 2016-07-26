tests
simpleEnsureTestWithNotification

	[self doSomething.
	self methodWithNotification.
	self doSomethingElse]
		ensure:
			[self doYetAnotherThing].
	