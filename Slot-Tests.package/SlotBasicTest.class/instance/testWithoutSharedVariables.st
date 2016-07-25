tests-shared variables
testWithoutSharedVariables

	aClass := self make: [ :builder | 
		builder sharedVariablesFromString: '' ].
	
	self assert: aClass classVarNames isEmpty