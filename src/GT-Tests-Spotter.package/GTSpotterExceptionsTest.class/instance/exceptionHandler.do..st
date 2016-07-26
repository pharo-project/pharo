private
exceptionHandler: anExceptionHandler do: aBlock
	self assertResetExceptions.
	self shouldnt: [ 
		anExceptionHandler handleExceptionsDuring: aBlock ] raise: Error.
	self assertNoExceptions.
	
	self assertResetExceptions.
	self shouldnt: [ 
		spotter exceptionHandler: anExceptionHandler.
		spotter handleExceptionsDuring: aBlock ] raise: Error.
	self assertNoExceptions.