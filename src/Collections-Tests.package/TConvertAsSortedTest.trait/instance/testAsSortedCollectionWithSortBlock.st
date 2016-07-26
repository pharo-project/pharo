tests - as sorted collection
testAsSortedCollectionWithSortBlock
	| result tmp |
	result := self collectionWithSortableElements  asSortedCollection: [:a :b | a > b].
	self assert: (result class includesBehavior: SortedCollection).
	result do: 
		[ :each | 
		self assert: (self collectionWithSortableElements   occurrencesOf: each) = (result occurrencesOf: each) ].
	self assert: result size = self collectionWithSortableElements  size.
	tmp:=result at: 1.
	result do: [:each| self assert: tmp>=each. tmp:=each].
	