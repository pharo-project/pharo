tests
testDeploymentHandleExceptionMock5
	
	self assertResetExceptions.
	self shouldnt: [ 
		[ 1 / 0 ] on: ZeroDivide do: [ :exception |
			self assert: (GTSpotterDeploymentExceptionHandlerMock5 new 
				handleException: exception 
				do: [ 7 ]) == 7 ] ] raise: ZeroDivide.
	self assertException: ZeroDivide