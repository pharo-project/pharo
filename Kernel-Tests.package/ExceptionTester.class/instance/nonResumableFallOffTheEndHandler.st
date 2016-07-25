tests
nonResumableFallOffTheEndHandler
	
	[self doSomething.
	MyTestError signal.
	self doSomethingElse]
		on: MyTestError
		do: [:ex | self doSomethingExceptional].
	self doYetAnotherThing