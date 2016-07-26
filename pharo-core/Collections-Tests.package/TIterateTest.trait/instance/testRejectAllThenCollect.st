tests - iterating
testRejectAllThenCollect
	| result |
	
	result := self collectionWithoutNilElements 
		reject: [ :each | each notNil ] 
		thenCollect: [ :each| self fail ].
	
	self assert: result isEmpty