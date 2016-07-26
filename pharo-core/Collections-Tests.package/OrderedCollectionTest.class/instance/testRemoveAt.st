tests - removing
testRemoveAt
	"Allows one to remove an element from a collection at an index"
	"self run:#testRemoveAt" 
	
	| c1 |
	c1 := #(2 3 4 6) asOrderedCollection.
	c1 removeAt: 2.
	self assert: (c1 = #(2 4 6) asOrderedCollection).
	self should: [c1 removeAt: 10] raise: Error.
	self should: [c1 removeAt: -1] raise: Error.
	