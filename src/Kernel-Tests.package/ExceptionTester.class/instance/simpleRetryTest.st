signaledexception tests
simpleRetryTest

	| theMeaningOfLife |
	theMeaningOfLife := nil.
	[self doSomething.
	theMeaningOfLife == nil
		ifTrue: [MyTestError signal]
		ifFalse: [self doSomethingElse]]
			on: MyTestError
			do:
				[:ex |
				theMeaningOfLife := 42.
				self doYetAnotherThing.
				ex retry]