signaledexception tests
simplePassTest

	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do:
			[:ex |
			self doYetAnotherThing.
			ex pass "expecting handler in #runTest:"]