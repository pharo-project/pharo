tests
testDeploymentExceptionHandlerMock4
	
	self assertResetExceptions.
	self shouldntRaise: ArithmeticError process: 'abc' origin: GTSpotterExceptionMock4 new.
	self assertException: ArithmeticError.
	
	self assertResetExceptions.
	self shouldntRaise: Error process: 'abc' origin: GTSpotterExceptionMock4 new.
	self assertException: ArithmeticError.