signaledexception tests
simpleReturnTest

	| it |
	it :=
		[self doSomething.
		MyTestError signal.
		self doSomethingElse]
			on: MyTestError
			do: [:ex | ex return: 3].
	it = 3 ifTrue: [self doYetAnotherThing]