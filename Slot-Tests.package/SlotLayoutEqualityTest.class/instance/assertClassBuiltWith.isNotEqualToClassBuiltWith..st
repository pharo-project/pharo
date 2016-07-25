helpers
assertClassBuiltWith: blockToBuildAClass isNotEqualToClassBuiltWith: blockToBuildAnotherClass
		
	aClass := self make: [ :builder | 
		builder 
			name: self aClassName;
			in: blockToBuildAClass ].
		
	anotherClass := self make: [ :builder | 
		builder 
			name: self anotherClassName;
			in: blockToBuildAnotherClass ].
	
	self assert: aClass classLayout ~= anotherClass classLayout.
	self assert: aClass classLayout hash ~= anotherClass classLayout hash.