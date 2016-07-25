tests
testDebuggingExceptionHandlerMock4

	self shouldRaise: ArithmeticError process: 'abc' origin: GTSpotterExceptionMock4 new do: [:e | 
		self assert: e messageText = 'Error while filtering' ].