tests
testDeploymentExceptionHandlerMock5
	
	self assertResetExceptions.
	self shouldntRaise: AssertionFailure process: 'abc' origin: GTSpotterExceptionMock5 new during: GTSpotterDeploymentExceptionHandlerMock new.
	self assertNoExceptions.