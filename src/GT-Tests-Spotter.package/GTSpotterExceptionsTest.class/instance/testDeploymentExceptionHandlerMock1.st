tests
testDeploymentExceptionHandlerMock1
	
	self assertResetExceptions.
	self shouldntRaise: AssertionFailure process: 'abc' origin: GTSpotterExceptionMock1 new.
	self assertException: AssertionFailure.
	
	self assertResetExceptions.
	self shouldntRaise: Error process: 'abc' origin: GTSpotterExceptionMock1 new.
	self assertException: AssertionFailure.