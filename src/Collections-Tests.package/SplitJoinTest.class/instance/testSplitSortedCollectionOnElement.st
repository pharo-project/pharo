running
testSplitSortedCollectionOnElement
	self assert: (((1 to: 10) asSortedCollection) splitOn: 4)
		equals: {(1 to: 3) asSortedCollection . (5 to: 10) asSortedCollection} asOrderedCollection.
