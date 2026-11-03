Extension { #name : 'TestExecutionEnvironmentTest' }

{ #category : '*Debugging-Utils-Tests' }
TestExecutionEnvironmentTest >> testNotifyTestServicesAboutHalt [

	| errorPassed expectedException |
	errorPassed := false.
	expectedException := Halt new messageText: 'test halt'.
	self runWithNoHandlers: [
			[ self runTestWith: [ expectedException signal ] ]
				on: Halt
				do: [ :actualException |
						errorPassed := true.
						self assert: actualException equals: expectedException.
						self assert: (self testService signaledExceptions includes: actualException) ] ].

	self assert: errorPassed
]
