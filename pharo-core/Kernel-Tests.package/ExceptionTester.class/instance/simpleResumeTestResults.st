signaledexception results
simpleResumeTestResults

	"see if we can resume twice"

	^OrderedCollection new
			add: self doSomethingString;
			add: self doYetAnotherThingString;
			add: self doSomethingElseString;
			add: self doYetAnotherThingString;
			add: self doSomethingElseString;
			yourself