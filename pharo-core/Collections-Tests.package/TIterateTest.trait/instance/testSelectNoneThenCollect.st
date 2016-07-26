tests - iterating
testSelectNoneThenCollect
	| result |
	
	result := self collectionWithoutNilElements 
		select: [ :each | each isNil ] 
		thenCollect: [ :each| self fail ].
	
	self assert: result isEmpty