tests - iterating
testRejectThenDo

	| result index rejectIndex |
	
	index := 0.
	rejectIndex := 0.
	
	result := self collectionWithoutNilElements 
		reject: [ :each | 
			rejectIndex := rejectIndex + 1.
			"reject the first element"
			rejectIndex = 1 ]
		thenDo: [ :each | 
			self assert: each notNil.
			index := index + 1]	.
			
	self assert: result equals: self collectionWithoutNilElements.
	self assert: rejectIndex equals: self collectionWithoutNilElements size.
	self assert: index equals: self collectionWithoutNilElements size - 1.
	 