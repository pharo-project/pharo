tests
testDeploymentExceptionHandlerMock2
	
	self assertResetExceptions.	
	self shouldntRaise: MessageNotUnderstood process: 'abc' origin: GTSpotterExceptionMock2 new.
	self assertException: MessageNotUnderstood.
	
	self assertResetExceptions.
	self shouldntRaise: Error process: 'abc' origin: GTSpotterExceptionMock2 new.
	self assertException: MessageNotUnderstood.