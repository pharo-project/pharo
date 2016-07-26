tests - iterating
testRejectAllThenDo
	| result |
	
	result := self collectionWithoutNilElements 
		reject: [ :each | each notNil ] 
		thenDo: [ :each | self fail ].
	
	self assert: result equals: self collectionWithoutNilElements