tests - adding
testAddAllFirstUnlessAlreadyPresent
	"Allows one to add each element of an orderedCollection at the beginning of
	another orderedCollection preserving the order but no duplicate element"
	"self run:#testAddAllFirstUnlessAlreadyPresent" 
	
	| c1 c2 c3 |
	c1 := #(1 2 3 4 ) asOrderedCollection.
	c2 := #(5 6 7 8 9 ) asOrderedCollection.
	c3 := #(0 1 ) asOrderedCollection.
	c2 addAllFirstUnlessAlreadyPresent: c1.
	self assert: c2 = #(1 2 3 4 5 6 7 8 9 ) asOrderedCollection.
	c1 addAllFirstUnlessAlreadyPresent: c3.
	self deny: c1 = #(0 1 1 2 3 4 ) asOrderedCollection.
	self assert: c1 = #(0 1 2 3 4 ) asOrderedCollection.
	