signaledexception results
simpleResignalAsTestResults

	^OrderedCollection new
		add: self doSomethingString;
		add: 'Unhandled Exception';
		yourself