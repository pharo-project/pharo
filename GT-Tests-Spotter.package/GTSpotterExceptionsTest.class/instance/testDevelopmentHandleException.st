tests
testDevelopmentHandleException

	self assertResetExceptions.
	self should: [
		[ 1 / 0 ] on: ZeroDivide do: [ :exception |
			GTSpotterDebuggingExceptionHandlerMock new handleException: exception ] ] raise: ZeroDivide.
	self assertException: ZeroDivide.