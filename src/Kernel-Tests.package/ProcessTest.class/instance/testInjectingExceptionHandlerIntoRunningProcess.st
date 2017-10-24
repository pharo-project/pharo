tests - exception handlers
testInjectingExceptionHandlerIntoRunningProcess

	| error interceptedError process sema started interrupted |
	DefaultExecutionEnvironment beActive.
	error := Error new messageText: 'test error'.
	sema := Semaphore new.
	interrupted := true.
	process := [started := true. sema wait. error signal. interrupted := false ] newProcess.
	process resume.
	Processor yield.
	
	self assert: started.
	process on: Error do: [ :err | interceptedError := err ].
	
	sema signal.
	Processor yield.
	
	self assert: interceptedError == error.
	self assert: interrupted