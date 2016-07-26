tests
resumableFallOffTheEndHandler

	[self doSomething.
	MyTestNotification signal.
	self doSomethingElse]
		on: MyTestNotification
		do: [:ex | self doSomethingExceptional].
	self doYetAnotherThing