exceptionselector
- anotherException
	"Create an exception set containnig the receiver
	and anotherException as an exclusion."

	^ ExceptionSetWithExclusions new
		add: self;
		addExclusion: anotherException;
		yourself