tests - exception handlers
testInjectingExceptionHandlerIntoProcessWithArg

	| error interceptedError process interrupted processArg |
	DefaultExecutionEnvironment beActive.
	error := Error new messageText: 'test error'.
	interrupted := true.	
	process := [:arg | processArg := arg. error signal. interrupted := false ] newProcessWith: #(#arg).
	process on: Error do: [ :err | interceptedError := err ].
	process resume.
	Processor yield.

	self assert: interceptedError == error.
	self assert: interrupted.
	self assert: processArg equals: #arg.