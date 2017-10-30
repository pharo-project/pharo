tests - exception handlers
testInjectingExceptionHandlerFromProcessItself

	| error interceptedError process interrupted |
	DefaultExecutionEnvironment beActive.
	error := Error new messageText: 'test error'.
	interrupted := true.
	process := [ Processor activeProcess on: Error do: [ :err | interceptedError := err ].
		error signal. interrupted := false ] fork.
	
	Processor yield.

	self assert: interceptedError == error.
	self assert: interrupted 