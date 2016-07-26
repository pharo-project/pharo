tests
testDebuggingExceptionHandlerMock5
	
	self assertResetExceptions.
	self shouldntRaise: AssertionFailure process: 'abc' origin: GTSpotterExceptionMock5 new during: GTSpotterDebuggingExceptionHandlerMock new.
	self assertNoExceptions.