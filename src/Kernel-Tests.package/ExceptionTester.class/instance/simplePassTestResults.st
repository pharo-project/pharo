signaledexception results
simplePassTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: 'Unhandled Exception';
		yourself