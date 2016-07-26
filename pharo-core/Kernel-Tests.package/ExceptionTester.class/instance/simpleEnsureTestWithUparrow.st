tests
simpleEnsureTestWithUparrow

	[self doSomething.
	true ifTrue: [^nil].
	self doSomethingElse]
		ensure:
			[self doYetAnotherThing].
	