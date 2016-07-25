running
testSplitSortedCollectionOnSortedCollection
	self assert: (((1 to: 10) asSortedCollection) splitOn: ((4 to: 5) asSortedCollection))
		equals: {(1 to: 3) asSortedCollection . (6 to: 10) asSortedCollection} asOrderedCollection.

