tests
testDeploymentExceptionHandlerDuring
	| exceptionHandler |
	
	self exceptionHandler: (exceptionHandler := GTSpotterDeploymentExceptionHandlerMock new) do: [ 
		self shouldnt: [ GTCurrentSpotterExceptionHandler value ] raise: Error.
		self assert: GTCurrentSpotterExceptionHandler value notNil.
		self assert: GTCurrentSpotterExceptionHandler value == exceptionHandler ].