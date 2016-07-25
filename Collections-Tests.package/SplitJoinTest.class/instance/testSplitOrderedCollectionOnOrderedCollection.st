running
testSplitOrderedCollectionOnOrderedCollection
	self assert: (((1 to: 10) asOrderedCollection) splitOn: ((4 to: 5) asOrderedCollection))
		equals: {(1 to: 3) asOrderedCollection . (6 to: 10) asOrderedCollection} asOrderedCollection.
