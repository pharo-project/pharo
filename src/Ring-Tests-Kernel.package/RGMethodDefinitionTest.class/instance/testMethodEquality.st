testing
testMethodEquality

	| newMethod newClass |
	self assert: (OrderedCollection>>#size) asRingDefinition = (OrderedCollection>>#size) asRingDefinition.
	
	newMethod := ((OrderedCollection>>#size) asRingDefinition)
					sourceCode: 'size
						^0'.
	self assert: ((OrderedCollection>>#size) asRingDefinition = newMethod).
	
	newClass:= RGClassDefinition named: #OrderedCollection.
	newMethod:= (RGMethodDefinition named: #size) parent: newClass.
	self assert: ((OrderedCollection>>#size) asRingDefinition = newMethod).
	
	newMethod:= (RGMethodDefinition named: #size) parent: (SortedCollection asRingDefinition).
	self deny: ((OrderedCollection>>#size) asRingDefinition = newMethod).