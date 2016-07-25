helpers
assertClassBuiltWith: blockToBuildAClass isEqualToClassBuiltWith: blockToBuildAnotherClass
	
	aClass := self make: [ :builder | 
		builder 
			name: self aClassName;
			in: blockToBuildAClass ].
		
	anotherClass := self make: [ :builder | 
		builder 
			name: self anotherClassName;
			in: blockToBuildAnotherClass ].
	
	self assert: aClass classLayout equals: aClass classLayout.	
	self assert: aClass classLayout equals: anotherClass classLayout.
	self assert: aClass classLayout hash equals: anotherClass classLayout hash.