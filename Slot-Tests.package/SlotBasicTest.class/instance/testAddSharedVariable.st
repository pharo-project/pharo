tests-shared variables
testAddSharedVariable

	self make: [ :builder | 
		builder sharedVariablesFromString: '' ].

	aClass := self make: [ :builder | 
		builder sharedVariablesFromString: 'Var' ].
	
	self assert: aClass classVarNames equals: #(Var).
