tests
testDeploymentExceptionHandlerMock3
	
	self assertResetExceptions.
	self shouldntRaise: ZeroDivide process: 'abc' origin: GTSpotterExceptionMock3 new.
	self assertException: ZeroDivide.
	
	self assertResetExceptions.
	self shouldntRaise: Error process: 'abc' origin: GTSpotterExceptionMock3 new.
	self assertException: ZeroDivide.