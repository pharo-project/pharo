signaledexception tests
simpleResumeTest

	"see if we can resume twice"

	
	[ | it |self doSomething.
	it := MyResumableTestError signal.
	it = 3 ifTrue: [self doSomethingElse].
	it := MyResumableTestError signal.
	it = 3 ifTrue: [self doSomethingElse].
	]
		on: MyResumableTestError
		do:
			[:ex |
			self doYetAnotherThing.
			ex resume: 3]