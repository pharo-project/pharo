signaledexception tests
simpleRetryUsingTest

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do:
			[:ex | ex retryUsing: [self doYetAnotherThing]]