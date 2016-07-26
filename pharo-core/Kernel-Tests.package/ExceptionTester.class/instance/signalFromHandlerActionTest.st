tests
signalFromHandlerActionTest

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do:
			[self doYetAnotherThing.
			MyTestError signal]