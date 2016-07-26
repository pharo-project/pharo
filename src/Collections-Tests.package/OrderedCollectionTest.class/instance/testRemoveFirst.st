tests - removing
testRemoveFirst
	"Allows one to remove n element of a collection at the first"
	"self run:#testRemoveFirst" 
	
	| c1 |
	c1 := #(2 3 4 6) asOrderedCollection.
	c1 removeFirst: 1.
	self assert: (c1 = #(3 4 6) asOrderedCollection).
	c1 removeFirst: 2.
	self assert: (c1 = #(6) asOrderedCollection).
	self should: [c1 removeFirst: 10] raise: Error.
	
	