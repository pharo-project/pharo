tests - adding
testAddAll
	"Allows one to add each element of an orderedCollection at the end of another
	orderedCollection "
	"self run:#testAddAll" 
	
	| c1 c2 |
	c1 := #(1 2 3 4 ) asOrderedCollection.
	c2 := #(5 6 7 8 9 ) asOrderedCollection.
	c1 addAll: c2.
	self assert: c1 = #(1 2 3 4 5 6 7 8 9) asOrderedCollection