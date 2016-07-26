private
runExample: example
	" this is not the testing tool we have in mind, but a temporary solution to integrate examples into the currently existing testing tools "
	| exampleResult |
	example ignoreTest ifTrue: [ ^ self ].
	self shouldnt: [ exampleResult := example run ] raise: Error.
	exampleResult isError 
		ifTrue: [ ^ self classForTestResult signalErrorWith: exampleResult gtDisplayString  ].
	exampleResult isFailure 
		ifTrue: [ ^ self classForTestResult signalFailureWith: exampleResult gtDisplayString  ] 