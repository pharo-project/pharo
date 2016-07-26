tests
testDebuggingExceptionHandlerDuring
	| exceptionHandler |
	
	self exceptionHandler: (exceptionHandler := GTSpotterDebuggingExceptionHandlerMock new) do: [ 
		self shouldnt: [ GTCurrentSpotterExceptionHandler value ] raise: Error.
		self assert: GTCurrentSpotterExceptionHandler value notNil.
		self assert: GTCurrentSpotterExceptionHandler value == exceptionHandler ].