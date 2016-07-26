tests-shared variables
testRemoveSharedVariable

	self make: [ :builder | 
		builder sharedVariablesFromString: 'Var' ].

	aClass := self make: [ :builder | 
		builder sharedVariablesFromString: '' ].
	
	self assert: aClass classVarNames isEmpty