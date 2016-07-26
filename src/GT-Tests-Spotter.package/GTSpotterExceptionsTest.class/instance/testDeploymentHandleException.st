tests
testDeploymentHandleException

	self assertResetExceptions.
	self shouldnt: [ 
		[ 1 / 0 ] on: ZeroDivide do: [ :exception |
			self assert: (GTSpotterDeploymentExceptionHandlerMock new handleException: exception) notNil ] ] raise: ZeroDivide.
	self assertException: ZeroDivide.