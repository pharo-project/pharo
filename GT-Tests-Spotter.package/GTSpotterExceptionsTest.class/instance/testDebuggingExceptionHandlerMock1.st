tests
testDebuggingExceptionHandlerMock1
	
	self shouldRaise: AssertionFailure process: 'abc' origin: GTSpotterExceptionMock1 new do: [:e | 
		self assert: e messageText = GTSpotterExceptionMock1 name ].