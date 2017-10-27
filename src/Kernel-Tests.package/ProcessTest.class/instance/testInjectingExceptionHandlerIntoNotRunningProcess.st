tests - exception handlers
testInjectingExceptionHandlerIntoNotRunningProcess

	| error interceptedError process interrupted |
	DefaultExecutionEnvironment beActive.
	error := Error new messageText: 'test error'.
	interrupted := true.	
	process := [ error signal. interrupted := false ] newProcess.
	process on: Error do: [ :err | interceptedError := err ].
	process resume.
	Processor yield.

	self assert: interceptedError == error.
	self assert: interrupted