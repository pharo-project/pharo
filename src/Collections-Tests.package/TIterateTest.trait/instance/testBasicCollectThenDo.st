tests - iterating
testBasicCollectThenDo

	| result index |
	index := 0.
	result := self collectionWithoutNilElements 
		collect: [ :each | nil ]
		thenDo: [ :each | 
			self assert: each isNil.
			index := index + 1]	.
			
	self assert: result equals: self collectionWithoutNilElements.
	self assert: index equals: self collectionWithoutNilElements size.
	 