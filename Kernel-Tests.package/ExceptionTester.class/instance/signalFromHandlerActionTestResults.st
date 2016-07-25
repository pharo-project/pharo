results
signalFromHandlerActionTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: self doYetAnotherThingString;
		add: 'Unhandled Exception';
		yourself