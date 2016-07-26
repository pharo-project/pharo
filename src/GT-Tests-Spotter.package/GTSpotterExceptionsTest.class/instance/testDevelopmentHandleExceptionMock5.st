tests
testDevelopmentHandleExceptionMock5

	self assertResetExceptions.
	self should: [ 
		[ 1 / 0 ] on: ZeroDivide do: [ :exception |
			GTSpotterDebuggingExceptionHandlerMock new 
				handleException: exception 
				do: [ self fail: 'Ups, we shouldnt have reached this statement' ] ] ] raise: ZeroDivide.
	self assertException: ZeroDivide