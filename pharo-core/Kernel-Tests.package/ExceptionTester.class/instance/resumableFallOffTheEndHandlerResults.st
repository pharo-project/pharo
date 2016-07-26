results
resumableFallOffTheEndHandlerResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doSomethingExceptionalString;
		add: self doYetAnotherThingString;
		yourself