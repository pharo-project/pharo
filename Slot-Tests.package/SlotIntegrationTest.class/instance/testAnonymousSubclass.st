tests
testAnonymousSubclass
	
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			slots: #(x)
		].

	anotherClass := aClass newAnonymousSubclass.
	self deny: anotherClass == aClass.
	self assert: anotherClass isBehavior.
	self assert: anotherClass superclass == aClass.
	self deny: anotherClass name = aClass name.
	self assert: anotherClass allInstVarNames = #(x)