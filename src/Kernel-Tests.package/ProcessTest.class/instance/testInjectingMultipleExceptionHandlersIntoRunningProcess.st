tests - exception handlers
testInjectingMultipleExceptionHandlersIntoRunningProcess

	| error process lastHandler firstHandler sema |
	DefaultExecutionEnvironment beActive.
	firstHandler := lastHandler := false.
	sema := Semaphore new.
	process := [ sema wait. error signal ] fork.
	Processor yield.
	process on: ZeroDivide do: [ :err | firstHandler := true ].
	process on: Error do: [ :err | lastHandler := true ].
	
	error := ZeroDivide new.
	sema signal.
	Processor yield.
	self assert: firstHandler.
	self deny: lastHandler.
	
	firstHandler := lastHandler := false.
	sema := Semaphore new.
	process := [ sema wait. error signal ] fork.
	Processor yield.
	process on: ZeroDivide do: [ :err | firstHandler := true ].
	process on: Error do: [ :err | lastHandler := true ].
	
	error := Error new.
	sema signal.
	Processor yield.
	self deny: firstHandler.
	self assert: lastHandler