Extension { #name : #ProcessTest }

{ #category : #'*Kernel-Tests-Extended' }
ProcessTest >> testInjectingExceptionHandlerFromProcessItself [

	| error interceptedError process interrupted |
	DefaultExecutionEnvironment beActive.
	error := Error new messageText: 'test error'.
	interrupted := true.
	process := [ Processor activeProcess on: Error do: [ :err | interceptedError := err ].
		error signal. interrupted := false ] fork.
	
	[process isTerminated] whileFalse: [ Processor yield ].

	self assert: interceptedError == error.
	self assert: interrupted 
]
