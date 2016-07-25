tests
testChangeSuperclass

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
	self assert: anotherClass superclass == aClass.

	"Change to a new superclass"
	yetAnotherClass := self make: [ :builder |
		builder 
			name: self yetAnotherClassName;
			superclass: Object ]. 

	anotherClass := self make: [ :builder |
		builder 
			name: self anotherClassName;
			superclass: yetAnotherClass ]. 
	
	self assert: aClass subclasses isEmpty.
	self assert: yetAnotherClass subclasses size equals: 1.
	self assert: yetAnotherClass subclasses anyOne == anotherClass.
	self assert: anotherClass superclass == yetAnotherClass.
