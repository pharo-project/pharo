testing
testUndeclared
	| undeclaredReferences |
	
	Smalltalk cleanOutUndeclared. 
	undeclaredReferences := Undeclared.
	
	self 
		assert: undeclaredReferences isEmpty 
		description: (String streamContents: [ :s|
			s 
				nextPutAll: 'Found undeclared references: ';
				print: undeclaredReferences ])
	