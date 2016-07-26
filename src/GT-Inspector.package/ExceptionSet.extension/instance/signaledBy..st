*GT-Inspector-Examples-Core
signaledBy: anException
	(anException isNil and: [ exceptions isEmptyOrNil not ]) ifTrue: [ 
		^ (GTExampleExpectedExceptionNotRaised new 
			expectedError: self;
			exception: anException;
			yourself) signal ].
	(anException notNil and: [ (exceptions includes: anException class) not ]) ifTrue: [ 
		^ (GTExampleUnexpectedExceptionRaised new
			unexpectedError: anException;
			yourself) signal ]