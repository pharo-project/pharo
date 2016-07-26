tests
testChangingFormatKeepsMethod

	aClass := self make: [ :builder | 
		builder 
			name: self aClassName;
			layoutClass: FixedLayout ].

	aClass compile: 'a ^self'.
	self assert: (aClass methodDictionary includesKey: #a).
	
	anotherClass := self make: [ :builder | 
		builder 
			name: self aClassName;
			layoutClass: VariableLayout .
			].

	self assert: (anotherClass methodDictionary includesKey: #a).
	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.