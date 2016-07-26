exceptionselector
- anotherException
	"Create an exception set containing the receiver 
	and anotherException as exclusion."

	^ ExceptionSetWithExclusions new
		add: self;
		addExclusion: anotherException;
		yourself