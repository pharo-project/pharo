tests
testAddSharedVariableKeepSubclasses
	"Reproduces fogbugz case 13028"

	"Define original hierarchy"
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			superclass: Object ]. 

	anotherClass := self make: [ :builder |
		builder 
			name: self anotherClassName;
			superclass: aClass ].

	self assert: aClass subclasses size equals: 1.
	self assert: aClass subclasses anyOne == anotherClass.

	"Add a shared variable"
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			superclass: Object;
			sharedVariablesFromString: 'Var' ]. 
	
	self assert: aClass subclasses size equals: 1.
	self assert: aClass subclasses anyOne == anotherClass.
