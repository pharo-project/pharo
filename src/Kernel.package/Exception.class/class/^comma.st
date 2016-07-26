exceptionselector
, anotherException
	"Create an exception set containing the receiver and anotherException"

	^ ExceptionSet new
		add: self;
		add: anotherException;
		yourself