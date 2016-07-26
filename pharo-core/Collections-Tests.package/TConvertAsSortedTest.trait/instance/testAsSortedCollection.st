tests - as sorted collection
testAsSortedCollection

	| aCollection result |
	aCollection := self collectionWithSortableElements .
	result := aCollection asSortedCollection.
	
	self assert: (result class includesBehavior: SortedCollection).
	result do: 
		[ :each | 
		self assert: (aCollection occurrencesOf: each) = (result occurrencesOf: each) ].
	
	self assert: result size = aCollection size.