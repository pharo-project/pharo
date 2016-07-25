tests - removing
testRemoveLast
	"Allows one to remove n element of a collection at the end"
	"self run:#testRemoveLast" 
	
	| c1 |
	c1 := #(2 3 4 6) asOrderedCollection.
	c1 removeLast: 1.
	self assert: (c1 = #(2 3 4) asOrderedCollection).
	c1 removeLast: 2.
	self assert: (c1 = #(2) asOrderedCollection).
	self should: [c1 removeLast: 10] raise: Error.