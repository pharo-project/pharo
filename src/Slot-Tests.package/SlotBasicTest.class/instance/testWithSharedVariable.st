tests-shared variables
testWithSharedVariable

	aClass := self make: [ :builder | 
		builder sharedVariablesFromString: 'Var' ].
	aClass class 
		compile: 'var:x  Var:=x';
		compile: 'var  ^Var'.
	
	aClass var: true.
	
	self assert: aClass classVarNames equals: #(Var).
	self assert: aClass var.