tests - iterating
testRejectThenCollect

	| result index selectIndex pivot |
	
	index := 0.
	selectIndex := 0.
	pivot := self collectionWithoutNilElements anyOne.
	
	result := self collectionWithoutNilElements 
		reject: [ :each | 
			selectIndex := selectIndex + 1.
			"reject the first element"
			selectIndex = 1 ]
		thenCollect: [ :each | 
			self assert: each notNil.
			index := index + 1.
			pivot ].
			
	self assert: result ~= self collectionWithoutNilElements.
	self assert: selectIndex equals: self collectionWithoutNilElements size.
	self assert: index equals: self collectionWithoutNilElements size - 1.
	
	self assert: (self collectionWithoutNilElements occurrencesOf: pivot) equals: 1.
	"should be > 1 for standard collection and = 1 for those that do not allow exact duplicates"
	self assert: (result occurrencesOf: pivot) >= 1.
	 