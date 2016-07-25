tests - iterating
testSelectNoneThenDo
	| result |
	
	result := self collectionWithoutNilElements 
		select: [ :each | each isNil ] 
		thenDo: [ self fail ].
	
	self assert: result equals: self collectionWithoutNilElements