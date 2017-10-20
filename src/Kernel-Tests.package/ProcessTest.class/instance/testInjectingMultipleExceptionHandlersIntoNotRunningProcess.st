tests - exception handlers
testInjectingMultipleExceptionHandlersIntoNotRunningProcess

	| error process lastHandler firstHandler |
	DefaultExecutionEnvironment beActive.
	firstHandler := lastHandler := false.
	process := [ error signal ] newProcess.
	process on: ZeroDivide do: [ :err | firstHandler := true ].
	process on: Error do: [ :err | lastHandler := true ].
	
	error := ZeroDivide new.
	process resume.
	Processor yield.
	self assert: firstHandler.
	self deny: lastHandler.
	
	firstHandler := lastHandler := false.
	process := [ error signal ] newProcess.
	process on: ZeroDivide do: [ :err | firstHandler := true ].
	process on: Error do: [ :err | lastHandler := true ].
	
	error := Error new.
	process resume.
	Processor yield.
	self deny: firstHandler.
	self assert: lastHandler