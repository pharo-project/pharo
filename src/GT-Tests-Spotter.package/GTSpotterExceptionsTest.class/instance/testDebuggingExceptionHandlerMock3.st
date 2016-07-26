tests
testDebuggingExceptionHandlerMock3

	self shouldRaise: ZeroDivide process: 'abc' origin: GTSpotterExceptionMock3 new.
