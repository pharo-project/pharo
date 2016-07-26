results
simpleEnsureTestWithErrorResults

	^OrderedCollection new
		add: self doSomethingString;
		add: 'Unhandled Exception';
		add: self doYetAnotherThingString;
		yourself