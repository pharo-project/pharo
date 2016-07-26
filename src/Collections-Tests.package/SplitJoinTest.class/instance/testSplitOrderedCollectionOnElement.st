running
testSplitOrderedCollectionOnElement
	self assert: (((1 to: 10) asOrderedCollection) splitOn: 4)
		equals: {(1 to: 3) asOrderedCollection . (5 to: 10) asOrderedCollection} asOrderedCollection.
